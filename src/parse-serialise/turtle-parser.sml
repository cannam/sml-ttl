
structure TurtleIncrementalParser : RDF_INCREMENTAL_PARSER = struct

    open RdfNode
    open RdfTriple
    open Prefix
             
    open TurtleCodepoints

    type base_iri = BaseIri.t

    datatype stream_value =
             END_OF_STREAM |
             PARSE_ERROR of string |
             PARSE_OUTPUT of {
                 base : base_iri,
                 prefixes : prefix list,
                 triples : triple list
             } * (unit -> stream_value)

    (* individual tokens are read as codepoint sequences, but they're
       encoded back to utf8 strings when constructing nodes or iris *)
    type token = word list

    val tokenOfString = WdString.explodeUtf8
    val stringOfToken = WdString.implodeToUtf8

    val iriOfToken = Iri.fromWideString o WdString.implode
    val tokenOfIri = WdString.explode o Iri.toWideString

    structure TokenMap = RedBlackMapFn (struct
                                         type ord_key = token
                                         val compare = List.collate Word.compare
                                         end)

    type parse_data = {
        source : Source.t,                  (* contains mutable state *)
        base : token * token,               (* without filename, filename only *)
        prefixes : token TokenMap.map,      (* prefix -> expansion *)
        blankNodes : int TokenMap.map,      (* token -> blank node id *)
        newBase : base_iri,
        newTriples : triple list,
        newPrefixes : prefix list
    }

    type match_state = parse_data * token
    type parse_state = parse_data * node option
                      
    datatype 'a result = ERROR of string | OK of 'a

    type match_result = match_state result
    type parse_result = parse_state result

    fun fromAscii a = Word.fromInt (Char.ord a)

    fun resolveIri (data : parse_data, token) =
        let val (base_iri, filePart) = #base data
            fun likeAbsoluteIri [] = false
              | likeAbsoluteIri (first::rest) = 
                if CodepointSet.contains alpha first
                then likeAbsoluteIri rest
                else first = fromAscii #":"
        in
            iriOfToken
                (case token of
                     [] => base_iri
                   | first::rest =>
                     if first = fromAscii #"#"
                     then base_iri @ filePart @ token
                     else if first = fromAscii #"/" then base_iri @ rest
                     else if likeAbsoluteIri token then token
                     else base_iri @ token)
        end

    fun addTriple (d : parse_data) (t : triple) =
        {
          source = #source d,
          base = #base d,
          prefixes = #prefixes d,
          blankNodes = #blankNodes d,
          newBase = #newBase d,
          newTriples = t :: #newTriples d,
          newPrefixes = #newPrefixes d
        }
                     
    fun addPrefix (d : parse_data) (p, e) =
        {
          source = #source d,
          base = #base d,
          prefixes = TokenMap.insert (#prefixes d, p, e),
          blankNodes = #blankNodes d,
          newBase = #newBase d,
          newTriples = #newTriples d,
          newPrefixes = (stringOfToken p, resolveIri (d, e)) ::
                         (#newPrefixes d)
        }
                     
    fun addBnode (d : parse_data) (b, id) =
        {
          source = #source d,
          base = #base d,
          prefixes = #prefixes d,
          blankNodes = TokenMap.insert (#blankNodes d, b, id),
          newBase = #newBase d,
          newTriples = #newTriples d,
          newPrefixes = #newPrefixes d
        }

    fun emitWithSubject (d : parse_data, subject, polist) =
        foldl (fn ((predicate, object), data) =>
                  addTriple data (subject, predicate, object))
              d polist

    fun composePartial (f, g) =
        fn a => 
           case f a of
               OK result => g result
             | ERROR e => ERROR e

    fun sequence s [] = OK s
      | sequence s funcs =
        (* e.g. [ f, g, h ] -> composePartial (h, composePartial (g, f)) *)
        let val rf = rev funcs
        in (foldl composePartial (hd rf) (tl rf)) s
        end
                                
    fun newBooleanLiteral b =
        LITERAL {
            value = if b then "true" else "false",
            lang = "",
            dtype = RdfStandardIRIs.iriTypeBoolean
        }
    val trueToken = tokenOfString "true"
    val falseToken = tokenOfString "false"
			      
    fun blankNodeFor (d: parse_data, token) =
	case TokenMap.find (#blankNodes d, token) of
	    SOME id => (d, BLANK id)
	  | NONE =>
	    case newBlankNode () of
		BLANK id => (addBnode d (token, id), BLANK id)
              | _ => raise Fail "newBlankNode returned non-blank node"

    fun peek (d : parse_data, _) = Source.peek (#source d)
    fun peekN n (d : parse_data, _) = Source.peekN n (#source d)
    fun read (d : parse_data, _) = Source.read (#source d)
    fun readN n (d : parse_data, _) = Source.readN n (#source d)
    fun location (d : parse_data, _) = Source.location (#source d)
    fun eof (d : parse_data, _) = Source.eof (#source d)

    fun lookingAt cps st =
        not (eof st) andalso CodepointSet.contains cps (peek st)

    fun lookingAtToken tok st =
	peekN (List.length tok) st = tok

    fun peekTurtle s =
        let val w = peek s in
            case CharMap.find (significantCharMap, w) of
                SOME significant => significant
              | NONE => C_NOTHING_INTERESTING
        end
						  
    fun mismatchMessage cps found =
        "expected " ^ (CodepointSet.name cps) ^ ", found \"" ^
        (stringOfToken [found]) ^ "\""
						  
    fun mismatchMessageTurtle c found =
        "expected " ^ (significantCharName c) ^ ", found \"" ^
        (stringOfToken [found]) ^ "\""

    fun splitAt (lst, elt) =
        let fun split' ([], acc) = NONE
              | split' (x::xs, acc) = if x = elt then SOME (rev acc, xs)
                                      else split' (xs, x::acc)
        in
            split' (lst, [])
        end

    fun prefixExpand (d, token) =
        let fun unescapeLocalPercent acc _ [] = ERROR "hex digits expected"
              | unescapeLocalPercent acc _ [a] = ERROR "two hex digits expected"
              | unescapeLocalPercent acc pc (a::b::rest) =
            (* "%-encoded sequences are in the character range for
                IRIs and are explicitly allowed in local names. These
                appear as a '%' followed by two hex characters and
                represent that same sequence of three
                characters. These sequences are not decoded during
                processing." [see also matchPercentEscape] *)
                if CodepointSet.contains hex a andalso
                   CodepointSet.contains hex b
                then (* remember we're building the string in reverse *)
                    unescapeLocalTail (b::a::pc::acc) rest
                else ERROR "hex digits expected after %" 

            and unescapeLocalBackslash acc _ [] = ERROR "escaped char expected"
              | unescapeLocalBackslash acc bs (first::rest) =
                if CodepointSet.contains pnameLocalEscapable first
                then unescapeLocalTail (first::acc) rest
                else ERROR "escapable character expected"
                           
            and unescapeLocalTail acc [] = OK acc
              | unescapeLocalTail acc [single] =
                if CodepointSet.contains pnameCharOrColon single
                then OK (single::acc)
                else ERROR ("invalid trailing character \"" ^
                            (stringOfToken [single]) ^
                            "\" in local part of name")
              | unescapeLocalTail acc (first::rest) =
                if CodepointSet.contains pnameCharColonOrDot first
                then unescapeLocalTail (first::acc) rest
                else if CodepointSet.contains pnameCharPercent first
                then unescapeLocalPercent acc first rest
                else if CodepointSet.contains pnameCharBackslash first
                then unescapeLocalBackslash acc first rest
                else ERROR ("unexpected character \"" ^
                            (stringOfToken [first]) ^
                            "\" in local part of name")

            and unescapeLocal [] = OK [] (* empty local part is allowed *)
              | unescapeLocal (first::rest) =
                if CodepointSet.contains pnameCharInitialLocal first
                then unescapeLocalTail [first] rest
                else if CodepointSet.contains pnameCharPercent first
                then unescapeLocalPercent [] first rest
                else if CodepointSet.contains pnameCharBackslash first
                then unescapeLocalBackslash [] first rest
                else ERROR ("unexpected character \"" ^
                            (stringOfToken [first]) ^ 
                            "\" in local part of name")

            and prefixExpand' (pre, post) =
                (* We don't check the prefix for well-formedness,
                   because if it isn't well-formed, it won't match
                   anything in our namespace map -- because we did
                   check for well-formedness when making the map.  We
                   do check the local part (post) for well-formedness,
                   while unescaping it. *)
                case TokenMap.find (#prefixes d, pre) of
                    NONE => ERROR ("unknown namespace prefix \"" ^
                                   (stringOfToken pre) ^ "\"")
                  | SOME ex =>
                    case unescapeLocal post of
                        ERROR err => ERROR err
                      | OK reversed =>
                        OK (d, SOME (IRI (resolveIri (d, ex @ (rev reversed)))))
                
        in
            case splitAt (token, fromAscii #":") of
                NONE => ERROR "prefixed name expected"
              | SOME (pre, post) => prefixExpand' (pre, post)
        end

    (* 
     types of reader function: discard, require, match, parse

     discard -> reads 0 or more of something, throws it away, never fails

     require -> reads 1 or more of something, throws it away, fails if not present

     match -> reads something, appends it to token part of match_state
              tuple, fails if not matched

     parse -> reads something, possibly stashes it in parse_data
              and/or returns in parse_state, fails if not matched
     *)

    fun discard (d : parse_data, t) = (Source.discard (#source d); (d, t))

    fun discardGreedy cps s =
        if eof s then OK s
        else
            if CodepointSet.contains cps (peek s)
            then discardGreedy cps (discard s)
            else OK s
            
    fun discardGreedyTurtle c s =
        if eof s then OK s
        else
            if peekTurtle s = c
            then discardGreedyTurtle c (discard s)
            else OK s
                                     
    fun discardToEol s =
        if eof s then OK s
        else
            if CodepointSet.contains eol (read s)
            then discardGreedy eol s
            else discardToEol s

    fun discardWhitespace s =
        if eof s then OK s
        else
            let val c = peek s in
                if CodepointSet.contains comment c then
                    case discardToEol (discard s) of
                        OK s => discardWhitespace s
                      | ERROR e => ERROR e
                else if CodepointSet.contains whitespaceEol c then
                    discardWhitespace (discard s)
                else OK s
            end

    fun require cps s =
        if eof s then ERROR "unexpected end of input"
        else let val c = read s in
                 if CodepointSet.contains cps c then OK s
                 else ERROR (mismatchMessage cps c)
             end

    fun requireNothing s = OK s
                 
    fun requireTurtle c s =
        if eof s then ERROR "unexpected end of input"
        else if CharMap.find (significantCharMap, peek s) = SOME c
        then OK (discard s)
        else ERROR (mismatchMessageTurtle c (peek s))

    fun requireWhitespace s =
	if lookingAt whitespaceEol s orelse
	   lookingAt comment s
	then discardWhitespace s
	else ERROR "whitespace expected"
		
    fun requirePunctuation c s =
        sequence s [ discardWhitespace, requireTurtle c ]

    fun match cps (s as (d, tok)) : match_result =
        let val w = read s in
            if CodepointSet.contains cps w
            then OK (d, tok @ [w])
            else ERROR (mismatchMessage cps w)
        end

    fun matchTurtle c (s as (d, tok)) : match_result =
        let val w = read s in
            if CharMap.find (significantCharMap, w) = SOME c
            then OK (d, tok @ [w])
            else ERROR (mismatchMessageTurtle c w)
        end

    (* May return an empty token *)
    fun matchToken cps (s as (d, tok)) : match_result =
        let fun match' () =
                if eof s then []
                else
                    if CodepointSet.contains cps (peek s)
                    then (read s) :: (match' ())
                    else []
        in
            OK (d, tok @ (match' ()))
        end

    (* May return an empty token *)
    fun matchTokenExcl cps (s as (d, tok)) : match_result =
        let fun match' () =
                if eof s orelse CodepointSet.contains cps (peek s)
                then []
                else (read s) :: (match' ())
        in
            OK (d, tok @ (match' ()))
        end

    (* Read something structured like a prefixed name. The caller is
       expected to test whether the result is properly formed depending
       on context. *)
    fun matchPrefixedNameCandidate s : match_result =
	let fun match' s =
		case matchTokenExcl pnameExcluded s of
		    ERROR e => ERROR e
		  | OK (s as (d, [])) => ERROR "token expected"
		  | OK (s as (d, token)) =>
                    case peekTurtle s of
                        C_DOT => 
                        (case peekN 2 s of
                             dot::next::[] =>
                             if CodepointSet.contains pnameAfterDot next
                             then let val c = read s (* the dot *) in
				      match' (d, token @ [c])
                                  end
                             else OK (d, token)
                           | anythingElse => OK (d, token))
                      | C_BACKSLASH =>
                     (* preserve the backslash and the following
                        character (even if it is in pnameExcluded)
                        and let the caller check that we have a valid
                        escape sequence *)
                        (case peekN 2 s of
                             slash::next::[] =>
                             let val esc = readN 2 s
                             in match' (d, token @ esc)
                             end
                          | anythingElse => ERROR "escaped character expected")
                      | anythingElse => OK (d, token)
	in
	    match' s
	end

    fun matchPercentEscape s : match_result =
        (* "%-encoded sequences are in the character range for IRIs
            and are explicitly allowed in local names. These appear as
            a '%' followed by two hex characters and represent that
            same sequence of three characters. These sequences are not
            decoded during processing." [see also prefixExpand] *)
        sequence s [ matchTurtle C_PERCENT,
                     match hex,
                     match hex ]
        
    fun unescapeUnicodeEscape s = (* !!! inconsistent name with matchPercentEscape *)
        let val n = case peekTurtle s of
                        C_LC_U => 4
                      | C_UC_U => 8
                      | other => 0
            val _ = discard s
            val u = readN n s
            val ustr = stringOfToken u
        in
            case Word.fromString ("0wx" ^ ustr) of
                SOME w => (s, w)
              | NONE => (s, 0wx0)
        end
            
    fun matchIriref s : match_result =
        let fun match' s =
                case matchTokenExcl iriExcluded s of
                    ERROR e => ERROR e
                  | OK (s as (d, token)) => 
                    case peekTurtle s of
                        C_PERCENT =>
                        (case matchPercentEscape (d, []) of
                             ERROR e => ERROR e
                           | OK (d, pe) => match' (d, token @ pe))
                      | C_BACKSLASH =>
                        (discard s;
                         if lookingAt unicodeU s
                         then let val (s, w) = unescapeUnicodeEscape s
                              in if w = 0wx0
                                 then ERROR "invalid Unicode escape"
                                 else if CodepointSet.contains iriExcluded w
                                 then ERROR ("illegal Unicode escaped character " ^
                                             "codepoint 0x" ^ (Word.toString w) ^
                                             " in IRI")
                                 else match' (d, token @ [w])
                              end
                         else ERROR "expected Unicode escape")
                      | other => OK (d, token)
        in
            sequence s [ requireTurtle C_OPEN_ANGLE,
                         match',
                         requireTurtle C_CLOSE_ANGLE ]
        end

    fun matchPrefixedNameNamespace s : match_result =
	case matchPrefixedNameCandidate s of
	    ERROR e => ERROR e
	  | OK (d, []) => ERROR "malformed prefix"
	  | OK (d, [w]) => if w = fromAscii #":"
			           then OK (d, [])
			           else ERROR "expected \":\" at end of prefix"
	  | OK (d, all) => 
	    let val first = hd all
		val r = rev all
		val colon = hd r
		val last = hd (tl r)
		val token = rev (tl r)
	    in
		if colon = fromAscii #":"
		then
		    if CodepointSet.contains basePnameChar first
		       andalso List.all (CodepointSet.contains
					     pnameCharOrDot) token
		       andalso CodepointSet.contains pnameChar last
		    then OK (d, token)
		    else ERROR ("malformed prefix \"" ^ (stringOfToken token) ^ "\"")
		else ERROR "expected \":\" at end of prefix"
	    end

    datatype quote = NO_QUOTE |
                     SHORT_STRING of turtleSignificantChar |
                     LONG_STRING of turtleSignificantChar

    fun haveThree s =
	case peekN 3 s of [a,b,c] => a = b andalso b = c
			 | anythingElse => false

    fun matchQuote s =
	let fun shortOrLong s q =
		if haveThree s
		then LONG_STRING q
		else SHORT_STRING q
	in		
	    case peekTurtle s of
		C_QUOTE_DOUBLE => shortOrLong s C_QUOTE_DOUBLE
	      | C_QUOTE_SINGLE => shortOrLong s C_QUOTE_SINGLE
	      | other => NO_QUOTE
	end
                 
    fun unescapeStringEscape s =
        let val e = read s in
            case CharMap.find (stringEscapeMap, e) of
                SOME w => (s, w)
              | NONE => (s, e)
        end
		     
    fun matchStringBodyType q s : match_result =
        let
	    val quoteCodepoint =
                case q of
                    LONG_STRING C_QUOTE_SINGLE => longStringSingleExcluded
                  | LONG_STRING C_QUOTE_DOUBLE => longStringDoubleExcluded
                  | SHORT_STRING C_QUOTE_SINGLE => shortStringSingleExcluded
                  | SHORT_STRING C_QUOTE_DOUBLE => shortStringDoubleExcluded
                  | _ => raise Fail "non-quote passed to matchStringBodyType"

	    fun match' s =
                case matchTokenExcl quoteCodepoint s of
                    ERROR e => ERROR e
                  | OK (s as (d, body)) =>
                    if eof s then ERROR "end-of-file reached in string"
                    else if peekTurtle s = C_BACKSLASH
                    then (discard s;
                          if lookingAt stringEscape s
                          then let val (s, w) = unescapeStringEscape s
                               in match' (d, body @ [w])
                               end
                          else if lookingAt unicodeU s
                          then let val (s, w) = unescapeUnicodeEscape s
                               in if w = 0wx0
                                  then ERROR "invalid Unicode escape"
                                  else match' (d, body @ [w])
                               end
                          else ERROR "expected escape sequence")
                    else case q of
                             LONG_STRING _ => 
                             (if haveThree s then OK s
                              else match' (d, body @ [read s]))
                           | SHORT_STRING _ => OK s
                           | _ => raise Fail "non-quote passed to matchStringBodyType"

            val quoteSeq =
                case q of
                    LONG_STRING c => [requireTurtle c, requireTurtle c, requireTurtle c]
                  | SHORT_STRING c => [requireTurtle c]
                  | _ => raise Fail "non-quote passed to matchStringBodyType"
        in
            sequence s (quoteSeq @ [match'] @ quoteSeq)
        end
            
    fun matchStringBody s : match_result =
	case matchQuote s of
	    NO_QUOTE => ERROR "expected quotation mark"
	  | quote => matchStringBodyType quote s

    fun matchLanguageTag s : match_result =
        let fun match' s =
                case matchToken alpha s of
                    ERROR e => ERROR e
                  | OK (s as (d, token)) =>
                    case peekTurtle s of
                        C_DASH => match' (d, token @ [read s])
                      | other => OK s
        in
            sequence s [
                requireTurtle C_AT,
                match',
                fn (d, []) => ERROR "non-empty language tag expected"
                  | s => OK s
            ]
        end
            
    (* helper for piping match function output into parse function
       input *)
    fun matchParseSeq d matchSeq parseFn =
        case (sequence (d, []) matchSeq) of
            ERROR e => ERROR e
          | OK (d, token) => parseFn (d, token)
            
    fun parseBase isSparql d : parse_result =
        matchParseSeq d [
	    requireWhitespace,
            matchIriref,
            if isSparql then requireNothing else requirePunctuation C_DOT
        ] (fn (d, token) =>
              let val base_iri = resolveIri (d, token)
                  val base = tokenOfIri base_iri
              in
                  OK ({ source = #source d,
                        base = (base, []),
                        prefixes = #prefixes d,
                        blankNodes = #blankNodes d,
                        newBase = SOME base_iri,
                        newTriples = #newTriples d,
                        newPrefixes = #newPrefixes d
                      }, NONE)
              end)
            
    fun parsePrefix isSparql d : parse_result =
        matchParseSeq d [
	    requireWhitespace,
            matchPrefixedNameNamespace,
            requireWhitespace
        ] (fn (d, prefix) =>
              matchParseSeq d [
		  matchIriref,
                  if isSparql then requireNothing else requirePunctuation C_DOT
	      ] (fn (d, iri) => OK (addPrefix d (prefix, iri), NONE)))

    fun isPrefixTag token =
        "prefix" = String.implode (map Char.toLower
                                       (String.explode
                                            (stringOfToken token)))
    fun isBaseTag token =
        "base" = String.implode (map Char.toLower
                                     (String.explode
                                          (stringOfToken token)))
                        
    fun parseSparqlBase d : parse_result =
        matchParseSeq d [
            matchPrefixedNameCandidate
        ] (fn (d, token) =>
              if isBaseTag token then parseBase true d
              else ERROR "expected \"BASE\"")
	
    fun parseSparqlPrefix d : parse_result =
        matchParseSeq d [
            matchPrefixedNameCandidate
        ] (fn (d, token) =>
              if isPrefixTag token then parsePrefix true d
              else ERROR "expected \"PREFIX\"")
        
    fun parseDirective d : parse_result =
        let val s = (d, []) in
	    case peekTurtle s of
	        C_LETTER_B => parseSparqlBase d
	      | C_LETTER_P => parseSparqlPrefix d
	      | C_AT =>
                matchParseSeq d
                    [ requireTurtle C_AT, matchToken alpha ]
                    (fn (d, token) => 
                        case stringOfToken token of
                            "prefix" => parsePrefix false d
                          | "base" => parseBase false d
		          | other => ERROR ("expected \"prefix\" or \"base\" " ^
                                            "after @, not \"" ^ other ^ "\""))
	      | other => ERROR "expected @prefix, @base, PREFIX, or BASE"
        end
	    
    (* [15] collection ::= '(' object* ')' *)
    fun parseCollection d : parse_result =
	let fun readObjects acc d =
                case (discardWhitespace (d, []);
		      peekTurtle (d, [])) of
		    C_CLOSE_PAREN =>
		    (discard (d, []);
		     if null acc
		     then OK (d, SOME (IRI RdfStandardIRIs.iriRdfNil))
		     else let val c = CollectionExpander.collectionOfNodes acc
			      val d = foldl (fn (t, data) => addTriple data t) d c
			  in
			      OK (d, SOME (#1 (hd c)))
			  end)
		  | _ => case parseObject d of
			     ERROR e => ERROR e
			   | OK (d, SOME obj) => readObjects (acc @ [obj]) d
			   | OK (d, NONE) => ERROR "object expected"
	in
	    matchParseSeq d [
		requireTurtle C_OPEN_PAREN
	    ] (fn (d, _) => readObjects [] d)
        end

    and parseBlankNode d : parse_result =
	matchParseSeq d [
	    requireTurtle C_UNDERSCORE,
	    requireTurtle C_COLON,
	    matchPrefixedNameCandidate
	] (fn (d, candidate) => 
	      (* !!! check that we match the blank node pattern. that is:
                   one initialBnodeChar
                   zero or more pnameCharOrDot
                   if there were any of those, then finally one pnameChar *)
	      case blankNodeFor (d, candidate) of
		  (d, node) => OK (d, SOME node))
	    
    and parsePrefixedName d : parse_result =
        matchParseSeq d [
            matchPrefixedNameCandidate
        ] (fn (d, token) =>
              (* We can't tell the difference, until we get here,
                 between a prefixed name and the bare literals true
                 or false *)
              if token = trueToken
	      then OK (d, SOME (newBooleanLiteral true))
              else if token = falseToken
	      then OK (d, SOME (newBooleanLiteral false))
              else prefixExpand (d, token))

    and parseIriref d : parse_result =
        matchParseSeq d [
            matchIriref
        ] (fn (d, token) => 
	      OK (d, SOME (IRI (resolveIri (d, token)))))
            
    and parseIri d : parse_result = 
        if peekTurtle (d, []) = C_OPEN_ANGLE
        then parseIriref d
        else parsePrefixedName d

    and parseAOrPrefixedName d : parse_result =
        matchParseSeq d [
            matchPrefixedNameCandidate
        ] (fn (d, token) =>
	      if token = [ fromAscii #"a" ]
	      then OK (d, SOME (IRI RdfStandardIRIs.iriRdfType))
	      else prefixExpand (d, token))

    and parseBlankNodePropertyList d : parse_result =
	case requireTurtle C_OPEN_SQUARE (d, []) of
	    ERROR e => ERROR e
	  | OK (d, _) => 
	    case parsePredicateObjectList d of
                ERROR e => ERROR e
	      | OK (d, p) =>  (* p may legitimately be empty *)
		let val blankNode = newBlankNode ()
		    val d = emitWithSubject (d, blankNode, p)
		in
		    case requireTurtle C_CLOSE_SQUARE (d, []) of
			ERROR e => ERROR e
		      | OK (d, _) => OK (d, SOME blankNode)
		end

    (* [133s] BooleanLiteral ::= 'true' | 'false' *)
    and parseBooleanLiteral d : parse_result =
	if lookingAtToken trueToken (d, [])
	then OK (d, SOME (newBooleanLiteral true))
	else if lookingAtToken falseToken (d, [])
	then OK (d, SOME (newBooleanLiteral false))
	else ERROR "expected \"true\" or \"false\""

    and parseDatatype d : parse_result =
        matchParseSeq d [
	    requireTurtle C_CARET,
	    requireTurtle C_CARET
	] (fn (d, _) => parseIri d)

    and annotateRdfLiteral (d : parse_data, lit : RdfNode.literal) :
        parse_result =
        case peekTurtle (d, []) of
            C_AT =>
            matchParseSeq d [
                matchLanguageTag
            ] (fn (d, tag) =>
                  let val lang = stringOfToken tag
                  in
                      if #lang lit = "" orelse #lang lit = lang
                      then annotateRdfLiteral (d, {
                                                  value = #value lit,
					          lang = lang,
                                                  dtype = #dtype lit
                                              })
                      else ERROR "contradictory languages specified"
                  end)
          | C_CARET =>
            (case parseDatatype d of
                 ERROR e => ERROR e
               | OK (d, SOME (IRI dtype)) =>
                 if #dtype lit = Iri.empty orelse #dtype lit = dtype
                 then annotateRdfLiteral (d, {
                                             value = #value lit,
                                             lang = #lang lit,
                                             dtype = dtype
                                         })
                 else ERROR "contradictory datatypes specified"
               | other => ERROR "internal error")
          | other =>
            OK (d, SOME (LITERAL {
                              value = #value lit,
                              lang = #lang lit,
                              dtype = if #dtype lit = Iri.empty
                                      then RdfStandardIRIs.iriTypeString
                                      else #dtype lit
               }))
                      
    and parseRdfLiteral d : parse_result =
        matchParseSeq d [
            matchStringBody
        ] (fn (d, body) =>
              annotateRdfLiteral (d, {
				     value = stringOfToken body,
				     lang = "",
                                     dtype = Iri.empty
                                 }))
	
    and parseNumericLiteral d =
        let val point = fromAscii #"."

            val candidate =
                case matchToken number (d, []) of
                    ERROR e => []
                  | OK (s as (d, n0)) =>
                    case peekN 2 s of
                        [a,b] => if a = point andalso
                                    CodepointSet.contains
                                        numberAfterPoint b
                                 then
				     (discard s; (* the point *)
                                      case matchToken number (d, []) of
                                          OK (d, n1) => (n0 @ [point] @ n1)
					| ERROR e => [])
                                 else n0
                      | _ => n0

	    val candidateStr = stringOfToken candidate

	    val containsE =
		isSome (List.find
			    (CodepointSet.contains exponent) candidate)
	    val containsDot =
		isSome (List.find (fn c => c = fromAscii #".") candidate)

	    val dtype = if containsE then RdfStandardIRIs.iriTypeDouble
			else if containsDot then RdfStandardIRIs.iriTypeDecimal
			else RdfStandardIRIs.iriTypeInteger
        in
	    (* spec says we store the literal as it appears in the
               file, don't canonicalise: we only convert it to check
               that it really is a number. Because SML's number
               conversion is slightly different from the Turtle spec,
               we need the odd additional test as well *)
            if String.isSuffix "e" candidateStr orelse
               String.isSuffix "E" candidateStr
            then ERROR ("double numeric literal must have digits after exponent")
            else
	        case Real.fromString candidateStr of
		    SOME i => OK (d, SOME (LITERAL {
						value = candidateStr,
						lang = "",
						dtype = dtype
				 }))
	          | NONE => ERROR ("expected numeric literal, found \"" ^
			           candidateStr ^ "\"")
        end
            
    (* [13] literal ::= RDFLiteral | NumericLiteral | BooleanLiteral *)
    and parseLiteral d =
	case peekTurtle (d, []) of
	    C_QUOTE_SINGLE => parseRdfLiteral d
	  | C_QUOTE_DOUBLE => parseRdfLiteral d
	  | C_LETTER_T => parseBooleanLiteral d
	  | C_LETTER_F => parseBooleanLiteral d
	  | C_DOT => parseNumericLiteral d
	  | other => if CodepointSet.contains number (peek (d, []))
		     then parseNumericLiteral d
		     else (* not literal after all! *) ERROR "object node expected"
					
    and parseNonLiteralObject d =
	case peekTurtle (d, []) of
	    C_UNDERSCORE => parseBlankNode d
	  | C_OPEN_PAREN => parseCollection d
	  | C_OPEN_SQUARE => parseBlankNodePropertyList d
	  | other => parseIri d
                               
    and parseObject d =
        if lookingAt notALiteral (d, [])
        then parseNonLiteralObject d
        else parseLiteral d
                               
    and parseVerb d =
	if peekTurtle (d, []) = C_OPEN_ANGLE then parseIri d
	else parseAOrPrefixedName d
					    
    (* [7] predicateObjectList ::= verb objectList (';' (verb objectList)?)*
       NB we permit an empty list here; caller must reject if its rule
       demands predicateObjectList rather than predicateObjectList? *)

    and parsePredicateObjectList d =
	let
	    fun parseObjectList (d, nodes) =
                (* would be better for these if the require/discard functions just took d/source *)
		case (ignore (requireWhitespace (d, []));
                      parseObject d) of
		    ERROR e => ERROR e
		  | OK (d, NONE) => ERROR "object node not found"
		  | OK (d, SOME node) =>
		    if (discardWhitespace (d, []);
                        peekTurtle (d, [])) = C_COMMA
		    then (discard (d, []);
                          parseObjectList (d, node::nodes))
		    else OK (d, rev (node::nodes))

	    fun parseVerbObjectList d =
		case parseVerb d of
		    ERROR e => ERROR ("verb IRI not found: " ^ e)
		  | OK (d, SOME (IRI iri)) =>
		    (case parseObjectList (d, []) of
			 ERROR e => ERROR e
		       | OK (d, nodes) =>
			 OK (d, map (fn n => (IRI iri, n)) nodes))
		  | OK other => ERROR "IRI expected for verb"

	    and parsePredicateObjectList' (d, volist) =
                case (discardWhitespace (d, []);
		      peekTurtle (d, [])) of
                    C_DOT => OK (d, volist)
                  | C_CLOSE_SQUARE => OK (d, volist)
                  | other => 
		    case parseVerbObjectList d of
			ERROR e => ERROR e
		      | OK (d, vos) =>
			if (discardWhitespace (d, []);
			    peekTurtle (d, [])) = C_SEMICOLON
			then (discardGreedyTurtle C_SEMICOLON (d, []);
			      parsePredicateObjectList' (d, volist @ vos))
			else OK (d, volist @ vos)
	in
	    parsePredicateObjectList' (d, [])
	end
          
    (* [10] subject ::= iri | blank *)
    and parseSubjectNode d =
	case peekTurtle (d, []) of
	    C_UNDERSCORE => parseBlankNode d
	  | C_OPEN_PAREN => parseCollection d
	  | other => parseIri d

    (* [6] triples ::= subject predicateObjectList |
                       blankNodePropertyList predicateObjectList?
       Handles the blankNodePropertyList part of that alternation *)
    and parseBlankNodeTriples d =
	case parseBlankNodePropertyList d of
	    ERROR e => ERROR e
          (* !!! when do we return NONE from a parser? *)
	  | OK (d, NONE) => ERROR "node expected"
	  | OK (d, SOME blankSubject) =>
	    case parsePredicateObjectList d of
		ERROR e => ERROR e
	      | OK (d, polist) =>
                OK (emitWithSubject (d, blankSubject, polist))

    (* [6] triples ::= subject predicateObjectList |
                       blankNodePropertyList predicateObjectList?
       Handles the subject part of that alternation *)
    and parseSubjectTriples d =
        case parseSubjectNode d of
            ERROR e => ERROR e
          | OK (d, NONE) => ERROR "node expected"
          | OK (d, SOME (LITERAL _)) => ERROR "subject may not be a literal"
          | OK (d, SOME subjectNode) =>
            case parsePredicateObjectList d of
                ERROR e => ERROR e
              | OK (d, []) => ERROR "predicate missing"
              | OK (d, polist) =>
                OK (emitWithSubject (d, subjectNode, polist))
                                        
    and parseTriples d =
        if peekTurtle (d, []) = C_OPEN_SQUARE
	then parseBlankNodeTriples d
        else parseSubjectTriples d
                                        
    (* [2] statement ::= directive | triples '.' *)
    and parseStatement d =
        case discardWhitespace (d, []) of
            ERROR e => ERROR e
          | OK (s as (d, _)) => 
            if eof s then OK (d, NONE)
            else
                if peekTurtle s = C_AT
                   orelse
                   (peekTurtle s = C_LETTER_P andalso isPrefixTag (peekN 6 s))
                   orelse
                   (peekTurtle s = C_LETTER_B andalso isBaseTag (peekN 4 s))
                then parseDirective d
                else case parseTriples d of
                         ERROR e => ERROR e
               (* !!! inconsistency: this returns only one value, parseDirective returns three (with NONE) *)
                       | OK d =>
                         case requirePunctuation C_DOT (d, []) of
                             ERROR e => ERROR e
                           | OK _ => OK (d, NONE)

    fun extendedErrorMessage d e =
	let val message = e ^ " at " ^ (location (d, []))
	    val nextBit =
                case peekN 8 (d, []) of
                    [] => peekN 4 (d, [])
                  | p => p
	in
	    if nextBit = []
	    then message
	    else message ^ " (before \"" ^ (stringOfToken nextBit) ^ "...\")"
	end

    fun parseDocument (d : parse_data) : stream_value =
        let fun parse' (d : parse_data) = fn () =>
                              parseDocument
                                  {
                                    source = #source d,
                                    base = #base d,
                                    prefixes = #prefixes d,
                                    blankNodes = #blankNodes d,
                                    newBase = NONE,
                                    newTriples = [],
                                    newPrefixes = []
                                  }
        in
            case parseStatement d of
                OK (d, _) => if eof (d, []) andalso
                                null (#newTriples d) andalso
                                null (#newPrefixes d)
                             then END_OF_STREAM
                             else PARSE_OUTPUT ({ base = #newBase d,
                                                  prefixes = #newPrefixes d,
                                                  triples = #newTriples d
                                                }, parse' d)
              | ERROR e => PARSE_ERROR (extendedErrorMessage d e)
        end

(*!!! need extensive tests for base iris (i) read from the file; (ii)
      supplied to the parser as argument; (iii) absent entirely. in
      what circumstances should we truncate the base to make a
      relative uri? *)
            
    fun splitBase iri : (token * token) =
        let val iristring = Iri.toString iri
            fun slashFields s = String.fields (fn x => x = #"/") iristring
            and split' i = 
                case slashFields i of
                    [] => ("", i)
                  | [single] => (single, "")
                  | bits =>
                    ((String.concatWith "/" (rev (tl (rev bits)))) ^ "/",
                     hd (rev bits))
            val (base_iri, filePart) = split' iristring
        in
            (tokenOfString base_iri, tokenOfString filePart)
        end
            
    fun parse (base_iri, stream) =
        parseDocument {
            source = Source.fromStream stream,
            base = splitBase (getOpt (base_iri, Iri.empty)),
            prefixes = TokenMap.empty,
            blankNodes = TokenMap.empty,
            newBase = NONE,
            newTriples = [],
            newPrefixes = []
        }
            
end

structure TurtleParser = RdfParserFn(TurtleIncrementalParser)

