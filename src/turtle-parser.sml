
structure TurtleStreamParser : RDF_STREAM_PARSER = struct

    open RdfTriple
    open TurtleCodepoints

    type base_iri = string

    datatype stream_value =
             END_OF_STREAM |
             PARSE_ERROR of string |
             PARSE_OUTPUT of {
                 prefixes : prefix list,
                 triples : triple list
             } * (unit -> stream_value)

    (* individual tokens are read as codepoint sequences, but they're
       encoded back to utf8 strings when constructing nodes or iris *)
    type token = word list

    val token_of_string = WdString.explodeUtf8
    val string_of_token = WdString.implodeToUtf8

    val iri_of_token = Iri.fromWideString o WdString.implode
    val token_of_iri = WdString.explode o Iri.toWideString

    structure TokenMap = RedBlackMapFn (struct
                                         type ord_key = token
                                         val compare = List.collate Word.compare
                                         end)

    type parse_data = {
        source : Source.t,                  (* contains mutable state *)
        base : token * token,               (* without filename, filename only *)
        prefixes : token TokenMap.map,      (* prefix -> expansion *)
        blank_nodes : int TokenMap.map,     (* token -> blank node id *)
        new_triples : triple list,
        new_prefixes : prefix list
    }

    type match_state = parse_data * token
    type parse_state = parse_data * node option
                      
    datatype 'a result = ERROR of string | OK of 'a

    type match_result = match_state result
    type parse_result = parse_state result

    fun from_ascii a = Word.fromInt (Char.ord a)

    (* !!! pretty sure each of these is called only in one place, so
           maybe should just write them inline *)
                                    
    fun add_triple (d : parse_data) (t : triple) =
        {
          source = #source d,
          base = #base d,
          prefixes = #prefixes d,
          blank_nodes = #blank_nodes d,
          new_triples = t :: #new_triples d,
          new_prefixes = #new_prefixes d
        }
                     
    fun add_prefix (d : parse_data) (p, e) =
        {
          source = #source d,
          base = #base d,
          prefixes = TokenMap.insert (#prefixes d, p, e),
          blank_nodes = #blank_nodes d,
          new_triples = #new_triples d,
          new_prefixes = (string_of_token p, string_of_token e) ::
                         (#new_prefixes d)
        }
                     
    fun add_bnode (d : parse_data) (b, id) =
        {
          source = #source d,
          base = #base d,
          prefixes = #prefixes d,
          blank_nodes = TokenMap.insert (#blank_nodes d, b, id),
          new_triples = #new_triples d,
          new_prefixes = #new_prefixes d
        }

    fun emit_with_subject (d : parse_data, subject, polist) =
        foldl (fn ((predicate, object), data) =>
                  add_triple data (subject, predicate, object))
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
                                
    fun new_boolean_literal b =
        LITERAL {
            value = if b then "true" else "false",
            lang = "",
            dtype = RdfStandardIRIs.iri_type_boolean
        }
    val true_token = token_of_string "true"
    val false_token = token_of_string "false"
			      
    fun blank_node_for (d: parse_data, token) =
	case TokenMap.find (#blank_nodes d, token) of
	    SOME id => (d, BLANK id)
	  | NONE =>
	    case new_blank_node () of
		BLANK id => (add_bnode d (token, id), BLANK id)
              | _ => raise Fail "new_blank_node returned non-blank node"

    fun peek (d : parse_data, _) = Source.peek (#source d)
    fun peek_n n (d : parse_data, _) = Source.peek_n n (#source d)
    fun read (d : parse_data, _) = Source.read (#source d)
    fun read_n n (d : parse_data, _) = Source.read_n n (#source d)
    fun location (d : parse_data, _) = Source.location (#source d)
    fun eof (d : parse_data, _) = Source.eof (#source d)

    fun looking_at cps st =
        not (eof st) andalso CodepointSet.contains cps (peek st)

    fun looking_at_token tok st =
	peek_n (List.length tok) st = tok

    fun peek_ttl s =
        let val w = peek s in
            case CharMap.find (significant_char_map, w) of
                SOME significant => significant
              | NONE => C_NOTHING_INTERESTING
        end
						  
    fun mismatch_message cps found =
        "expected " ^ (CodepointSet.name cps) ^ ", found \"" ^
        (string_of_token [found]) ^ "\""
						  
    fun mismatch_message_ttl c found =
        "expected " ^ (significant_char_name c) ^ ", found \"" ^
        (string_of_token [found]) ^ "\""

    fun split_at (lst, elt) =
        let fun split' ([], acc) = NONE
              | split' (x::xs, acc) = if x = elt then SOME (rev acc, xs)
                                      else split' (xs, x::acc)
        in
            split' (lst, [])
        end

    fun resolve_iri (data : parse_data, token) =
        let val (base_iri, file_part) = #base data
            fun like_absolute_iri [] = false
              | like_absolute_iri (first::rest) = 
                if CodepointSet.contains alpha first
                then like_absolute_iri rest
                else if first = from_ascii #":"
                then case rest of
                         s1::s2::_ => s1 = s2 andalso s1 = from_ascii #"/"
                       | _ => false
                else false
        in
            iri_of_token
                (case token of
                     [] => base_iri
                   | first::rest =>
                     if first = from_ascii #"#"
                     then base_iri @ file_part @ token
                     else if first = from_ascii #"/" then base_iri @ rest
                     else if like_absolute_iri token then token
                     else base_iri @ token)
        end

    fun prefix_expand (d, token) =
        let fun unescape_local_tail acc [] = OK acc
              | unescape_local_tail acc [single] =
                if CodepointSet.contains pname_char_or_colon single
                then OK (single::acc)
                else ERROR ("invalid trailing character \"" ^
                            (string_of_token [single]) ^
                            "\" in local part of name")
              | unescape_local_tail acc (first::rest) =
                if CodepointSet.contains pname_char_colon_or_dot first
                then unescape_local_tail (first::acc) rest
                else if CodepointSet.contains pname_char_percent first
                then unescape_local_percent acc first rest
                else if CodepointSet.contains pname_char_backslash first
                then unescape_local_backslash acc first rest
                else ERROR ("unexpected character \"" ^
                            (string_of_token [first]) ^
                            "\" in local part of name")

            and unescape_local_percent acc _ [] = ERROR "hex digits expected"
              | unescape_local_percent acc _ [a] = ERROR "two hex digits expected"
              | unescape_local_percent acc pc (a::b::rest) =
            (* "%-encoded sequences are in the character range for
                IRIs and are explicitly allowed in local names. These
                appear as a '%' followed by two hex characters and
                represent that same sequence of three
                characters. These sequences are not decoded during
                processing." [see also match_percent_escape] *)
                if CodepointSet.contains hex a andalso
                   CodepointSet.contains hex b
                then (* remember we're building the string in reverse *)
                    unescape_local_tail (b::a::pc::acc) rest
                else ERROR "hex digits expected after %" 

            and unescape_local_backslash acc _ [] = ERROR "escaped char expected"
              | unescape_local_backslash acc bs (first::rest) =
                if CodepointSet.contains pname_local_escapable first
                then unescape_local_tail (first::acc) rest
                else ERROR "escapable character expected"
                           
            and unescape_local [] = ERROR "local part of name expected"
              | unescape_local (first::rest) =
                if CodepointSet.contains pname_char_initial_local first
                then unescape_local_tail [first] rest
                else if CodepointSet.contains pname_char_percent first
                then unescape_local_percent [] first rest
                else if CodepointSet.contains pname_char_backslash first
                then unescape_local_backslash [] first rest
                else ERROR ("unexpected character \"" ^
                            (string_of_token [first]) ^ 
                            "\" in local part of name")

            and prefix_expand' (pre, post) =
                (* We don't check the prefix for well-formedness,
                   because if it isn't well-formed, it won't match
                   anything in our namespace map -- because we did
                   check for well-formedness when making the map.  We
                   do check the local part (post) for well-formedness,
                   while unescaping it. *)
                case TokenMap.find (#prefixes d, pre) of
                    NONE => ERROR ("unknown namespace prefix \"" ^
                                   (string_of_token pre) ^ "\"")
                  | SOME ex =>
                    case unescape_local post of
                        ERROR err => ERROR err
                      | OK reversed =>
                        OK (d, SOME (IRI (resolve_iri (d, ex @ (rev reversed)))))
                
        in
            case split_at (token, from_ascii #":") of
                NONE => ERROR "prefixed name expected"
              | SOME (pre, post) => prefix_expand' (pre, post)
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

    fun discard_greedy cps s =
        if eof s then OK s
        else
            if CodepointSet.contains cps (peek s)
            then discard_greedy cps (discard s)
            else OK s
            
    fun discard_greedy_ttl c s =
        if eof s then OK s
        else
            if peek_ttl s = c
            then discard_greedy_ttl c (discard s)
            else OK s
                                     
    fun discard_to_eol s =
        if eof s then OK s
        else
            if CodepointSet.contains eol (read s)
            then discard_greedy eol s
            else discard_to_eol s

    fun discard_whitespace s =
        if eof s then OK s
        else
            let val c = peek s in
                if CodepointSet.contains comment c then
                    case discard_to_eol (discard s) of
                        OK s => discard_whitespace s
                      | ERROR e => ERROR e
                else if CodepointSet.contains whitespace_eol c then
                    discard_whitespace (discard s)
                else OK s
            end

    fun require cps s =
        if eof s then ERROR "unexpected end of input"
        else let val c = read s in
                 if CodepointSet.contains cps c then OK s
                 else ERROR (mismatch_message cps c)
             end

    fun require_nothing s = OK s
                 
    fun require_ttl c s =
        if eof s then ERROR "unexpected end of input"
        else if CharMap.find (significant_char_map, peek s) = SOME c
        then OK (discard s)
        else ERROR (mismatch_message_ttl c (peek s))

    fun require_whitespace s =
	if looking_at whitespace_eol s orelse
	   looking_at comment s
	then discard_whitespace s
	else ERROR "whitespace expected"
		
    fun require_punctuation c s =
        sequence s [ discard_whitespace, require_ttl c ]

    fun match cps (s as (d, tok)) : match_result =
        let val w = read s in
            if CodepointSet.contains cps w
            then OK (d, tok @ [w])
            else ERROR (mismatch_message cps w)
        end

    fun match_ttl c (s as (d, tok)) : match_result =
        let val w = read s in
            if CharMap.find (significant_char_map, w) = SOME c
            then OK (d, tok @ [w])
            else ERROR (mismatch_message_ttl c w)
        end

    (* May return an empty token *)
    fun match_token cps (s as (d, tok)) : match_result =
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
    fun match_token_excl cps (s as (d, tok)) : match_result =
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
    fun match_prefixed_name_candidate s : match_result =
	let fun match' s =
		case match_token_excl pname_excluded s of
		    ERROR e => ERROR e
		  | OK (s as (d, [])) => ERROR "token expected"
		  | OK (s as (d, token)) =>
                    case peek_ttl s of
                        C_DOT => 
                        (case peek_n 2 s of
                             dot::next::[] =>
                             if CodepointSet.contains pname_after_dot next
                             then let val c = read s (* the dot *) in
				      match' (d, token @ [c])
                                  end
                             else OK (d, token)
                           | anything_else => OK (d, token))
                      | C_BACKSLASH =>
                     (* preserve the backslash and the following
                        character (even if it is in pname_excluded)
                        and let the caller check that we have a valid
                        escape sequence *)
                        (case peek_n 2 s of
                             slash::next::[] =>
                             let val esc = read_n 2 s
                             in match' (d, token @ esc)
                             end
                          | anything_else => ERROR "escaped character expected")
                      | anything_else => OK (d, token)
	in
	    match' s
	end

    fun match_percent_escape s : match_result =
        (* "%-encoded sequences are in the character range for IRIs
            and are explicitly allowed in local names. These appear as
            a '%' followed by two hex characters and represent that
            same sequence of three characters. These sequences are not
            decoded during processing." [see also prefix_expand] *)
        sequence s [ match_ttl C_PERCENT,
                     match hex,
                     match hex ]
        
    fun unescape_unicode_escape s = (* !!! inconsistent name with match_percent_escape *)
        let val n = case peek_ttl s of
                        C_LC_U => 4
                      | C_UC_U => 8
                      | other => 0
            val _ = discard s
            val u = read_n n s
            val ustr = string_of_token u
        in
            case Word.fromString ("0wx" ^ ustr) of
                SOME w => (s, w)
              | NONE => (s, 0wx0)
        end
            
    fun match_iriref s : match_result =
        let fun match' s =
                case match_token_excl iri_escaped s of
                    ERROR e => ERROR e
                  | OK (s as (d, token)) => 
                    case peek_ttl s of
                        C_PERCENT =>
                        (case match_percent_escape (d, []) of
                             ERROR e => ERROR e
                           | OK (d, pe) => match' (d, token @ pe))
                      | C_BACKSLASH =>
                        (discard s;
                         if looking_at unicode_u s
                         then let val (s, w) = unescape_unicode_escape s
                              in if w = 0wx0
                                 then ERROR "invalid Unicode escape"
                                 else if CodepointSet.contains iri_escaped w
                                 then ERROR "illegal Unicode escaped character"
                                 else match' (d, token @ [w])
                              end
                         else ERROR "expected Unicode escape")
                      | other => OK (d, token)
        in
            sequence s [ require_ttl C_OPEN_ANGLE,
                         match',
                         require_ttl C_CLOSE_ANGLE ]
        end

    fun match_prefixed_name_namespace s : match_result =
	case match_prefixed_name_candidate s of
	    ERROR e => ERROR e
	  | OK (d, []) => ERROR "malformed prefix"
	  | OK (d, [w]) => if w = from_ascii #":"
			           then OK (d, [])
			           else ERROR "expected \":\" at end of prefix"
	  | OK (d, all) => 
	    let val first = hd all
		val r = rev all
		val colon = hd r
		val last = hd (tl r)
		val token = rev (tl r)
	    in
		if colon = from_ascii #":"
		then
		    if CodepointSet.contains base_pname_char first
		       andalso List.all (CodepointSet.contains
					     pname_char_or_dot) token
		       andalso CodepointSet.contains pname_char last
		    then OK (d, token)
		    else ERROR ("malformed prefix \"" ^ (string_of_token token) ^ "\"")
		else ERROR "expected \":\" at end of prefix"
	    end

    datatype quote = NO_QUOTE |
                     SHORT_STRING of turtle_significant_char |
                     LONG_STRING of turtle_significant_char

    fun have_three s =
	case peek_n 3 s of [a,b,c] => a = b andalso b = c
			 | anything_else => false

    fun match_quote s =
	let fun short_or_long s q =
		if have_three s
		then LONG_STRING q
		else SHORT_STRING q
	in		
	    case peek_ttl s of
		C_QUOTE_DOUBLE => short_or_long s C_QUOTE_DOUBLE
	      | C_QUOTE_SINGLE => short_or_long s C_QUOTE_SINGLE
	      | other => NO_QUOTE
	end

    fun match_long_string_body s q : match_result =
        let
	    val quote_codepoint =
		if q = C_QUOTE_SINGLE
		then long_string_single_excluded
		else long_string_double_excluded

	    fun match' s =
                case match_token_excl quote_codepoint s of
                    ERROR e => ERROR e
                  | OK (s as (d, token)) => (*!!! todo: handle escapes *)
                    if eof s then ERROR "end-of-file reached in string"
                    else if have_three s then OK s
                    else match' (d, token @ [read s])
        in
            sequence s [
                require_ttl q, require_ttl q, require_ttl q,
                match',
                require_ttl q, require_ttl q, require_ttl q
            ]
        end
                 
    fun unescape_string_escape s =
        let val e = read s in
            case CharMap.find (string_escape_map, e) of
                SOME w => (s, w)
              | NONE => (s, e)
        end
            
    fun match_short_string_body s q : match_result =
	let
	    val quote_codepoint =
		if q = C_QUOTE_SINGLE
		then short_string_single_excluded
		else short_string_double_excluded

	    fun match' s =
                case match_token_excl quote_codepoint s of
		    ERROR e => ERROR e
	          | OK (s as (d, body)) =>
                    if peek_ttl s = C_BACKSLASH
                    then (discard s;
                          if looking_at string_escape s
                          then let val (s, w) = unescape_string_escape s
                               in match' (d, body @ [w])
                               end
                          else if looking_at unicode_u s
                          then let val (s, w) = unescape_unicode_escape s
                               in if w = 0wx0
                                  then ERROR "invalid Unicode escape"
                                  else match' (d, body @ [w])
                               end
                          else ERROR "expected escape sequence")
                    else OK s
        in
            sequence s [
                require_ttl q,
                match',
                require_ttl q
            ]
        end
		     
    fun match_string_body s : match_result =
	case match_quote s of
	    NO_QUOTE => ERROR "expected quotation mark"
	  | SHORT_STRING q => match_short_string_body s q
	  | LONG_STRING q => match_long_string_body s q

    fun match_language_tag s : match_result =
        let fun match' s =
                case match_token alpha s of
                    ERROR e => ERROR e
                  | OK (s as (d, token)) =>
                    case peek_ttl s of
                        C_DASH => match' (d, token @ [read s])
                      | other => OK s
        in
            sequence s [
                require_ttl C_AT,
                match',
                fn (d, []) => ERROR "non-empty language tag expected"
                  | s => OK s
            ]
        end
            
    (* helper for piping match function output into parse function
       input *)
    fun match_parse_seq d match_seq parse_fn =
        case (sequence (d, []) match_seq) of
            ERROR e => ERROR e
          | OK (d, token) => parse_fn (d, token)
            
    fun parse_base is_sparql d : parse_result =
        match_parse_seq d [
	    require_whitespace,
            match_iriref,
            if is_sparql then require_nothing else require_punctuation C_DOT
        ] (fn (d, token) =>
              let val base = token_of_iri (resolve_iri (d, token))
              in
                  OK ({ source = #source d,
                        base = (base, []),
                        prefixes = #prefixes d,
                        blank_nodes = #blank_nodes d,
                        new_triples = #new_triples d,
                        new_prefixes = #new_prefixes d
                      }, NONE)
              end)
            
    fun parse_prefix is_sparql d : parse_result =
        match_parse_seq d [
	    require_whitespace,
            match_prefixed_name_namespace,
            require_whitespace
        ] (fn (d, prefix) =>
              match_parse_seq d [
		  match_iriref,
                  if is_sparql then require_nothing else require_punctuation C_DOT
	      ] (fn (d, iri) => OK (add_prefix d (prefix, iri), NONE)))

    fun is_prefix_tag token =
        "prefix" = String.implode (map Char.toLower
                                       (String.explode
                                            (string_of_token token)))
    fun is_base_tag token =
        "base" = String.implode (map Char.toLower
                                     (String.explode
                                          (string_of_token token)))
                        
    fun parse_sparql_base d : parse_result =
        match_parse_seq d [
            match_prefixed_name_candidate
        ] (fn (d, token) =>
              if is_base_tag token then parse_base true d
              else ERROR "expected \"BASE\"")
	
    fun parse_sparql_prefix d : parse_result =
        match_parse_seq d [
            match_prefixed_name_candidate
        ] (fn (d, token) =>
              if is_prefix_tag token then parse_prefix true d
              else ERROR "expected \"PREFIX\"")
        
    fun parse_directive d : parse_result =
        let val s = (d, []) in
	    case peek_ttl s of
	        C_LETTER_B => parse_sparql_prefix d
	      | C_LETTER_P => parse_sparql_prefix d
	      | C_AT =>
                match_parse_seq d
                    [ require_ttl C_AT, match_token alpha ]
                    (fn (d, token) => 
                        case string_of_token token of
                            "prefix" => parse_prefix false d
                          | "base" => parse_base false d
		          | other => ERROR ("expected \"prefix\" or \"base\" " ^
                                            "after @, not \"" ^ other ^ "\""))
	      | other => ERROR "expected @prefix, @base, PREFIX, or BASE"
        end
	    
    (* [15] collection ::= '(' object* ')' *)
    fun parse_collection d : parse_result =
	let fun read_objects acc d =
                case (discard_whitespace (d, []);
		      peek_ttl (d, [])) of
		    C_CLOSE_PAREN =>
		    (discard (d, []);
		     if null acc
		     then OK (d, SOME (IRI RdfStandardIRIs.iri_rdf_nil))
		     else let val c = RdfCollection.collection_of_nodes acc
			      val d = foldl (fn (t, data) => add_triple data t) d c
			  in
			      OK (d, SOME (#1 (hd c)))
			  end)
		  | _ => case parse_object d of
			     ERROR e => ERROR e
			   | OK (d, SOME obj) => read_objects (acc @ [obj]) d
			   | OK (d, NONE) => ERROR "object expected"
	in
	    match_parse_seq d [
		require_ttl C_OPEN_PAREN
	    ] (fn (d, _) => read_objects [] d)
        end

    and parse_blank_node d : parse_result =
	match_parse_seq d [
	    require_ttl C_UNDERSCORE,
	    require_ttl C_COLON,
	    match_prefixed_name_candidate
	] (fn (d, candidate) => 
	      (* !!! check that we match the blank node pattern. that is:
                   one initial_bnode_char
                   zero or more pname_char_or_dot
                   if there were any of those, then finally one pname_char *)
	      case blank_node_for (d, candidate) of
		  (d, node) => OK (d, SOME node))
	    
    and parse_prefixed_name d : parse_result =
        match_parse_seq d [
            match_prefixed_name_candidate
        ] (fn (d, token) =>
              (* We can't tell the difference, until we get here,
                 between a prefixed name and the bare literals true
                 or false *)
              if token = true_token
	      then OK (d, SOME (new_boolean_literal true))
              else if token = false_token
	      then OK (d, SOME (new_boolean_literal false))
              else prefix_expand (d, token))

    and parse_iriref d : parse_result =
        match_parse_seq d [
            match_iriref
        ] (fn (d, token) => 
	      OK (d, SOME (IRI (resolve_iri (d, token)))))
            
    and parse_iri d : parse_result = 
        if peek_ttl (d, []) = C_OPEN_ANGLE
        then parse_iriref d
        else parse_prefixed_name d

    and parse_a_or_prefixed_name d : parse_result =
        match_parse_seq d [
            match_prefixed_name_candidate
        ] (fn (d, token) =>
	      if token = [ from_ascii #"a" ]
	      then OK (d, SOME (IRI RdfStandardIRIs.iri_rdf_type))
	      else prefix_expand (d, token))

    and parse_blank_node_property_list d : parse_result =
	case require_ttl C_OPEN_SQUARE (d, []) of
	    ERROR e => ERROR e
	  | OK (d, _) => 
	    case parse_predicate_object_list d of
                ERROR e => ERROR e
	      | OK (d, p) =>  (* p may legitimately be empty *)
		let val blank_node = new_blank_node ()
		    val d = emit_with_subject (d, blank_node, p)
		in
		    case require_ttl C_CLOSE_SQUARE (d, []) of
			ERROR e => ERROR e
		      | OK (d, _) => OK (d, SOME blank_node)
		end

    (* [133s] BooleanLiteral ::= 'true' | 'false' *)
    and parse_boolean_literal d : parse_result =
	if looking_at_token true_token (d, [])
	then OK (d, SOME (new_boolean_literal true))
	else if looking_at_token false_token (d, [])
	then OK (d, SOME (new_boolean_literal false))
	else ERROR "expected \"true\" or \"false\""

    and parse_datatype d : parse_result =
        match_parse_seq d [
	    require_ttl C_CARET,
	    require_ttl C_CARET
	] (fn (d, _) => parse_iri d)
                   
    and parse_rdf_literal d : parse_result =
        match_parse_seq d [
            match_string_body
        ] (fn (d, body) =>
              case peek_ttl (d, []) of
                  C_AT =>
                  match_parse_seq d [
                      match_language_tag
                  ] (fn (d, tag) =>
                        OK (d, SOME (LITERAL {
					  value = string_of_token body,
					  lang = string_of_token tag,
					  dtype = Iri.empty
		    })))
                | C_CARET =>
                  (case parse_datatype d of
                       ERROR e => ERROR e
                     | OK (d, SOME (IRI tag)) =>
                       OK (d, SOME (LITERAL {
					 value = string_of_token body,
					 lang = "",
					 dtype = tag
			  }))
                     | other => ERROR "internal error")
                | other => OK (d, SOME (LITERAL {
					     value = string_of_token body,
					     lang = "",
					     dtype = Iri.empty
			      })))
	
    and parse_numeric_literal d =
        let val point = from_ascii #"."

            val candidate =
                case match_token number (d, []) of
                    ERROR e => []
                  | OK (s as (d, n0)) =>
                    case peek_n 2 s of
                        [a,b] => if a = point andalso
                                    CodepointSet.contains
                                        number_after_point b
                                 then
				     (discard s; (* the point *)
                                      case match_token number (d, []) of
                                          OK (d, n1) => (n0 @ [point] @ n1)
					| ERROR e => [])
                                 else n0
                      | _ => n0

	    val candidate_str = string_of_token candidate

	    val contains_e =
		isSome (List.find
			    (CodepointSet.contains exponent) candidate)
	    val contains_dot =
		isSome (List.find (fn c => c = from_ascii #".") candidate)

	    val dtype = if contains_e then RdfStandardIRIs.iri_type_double
			else if contains_dot then RdfStandardIRIs.iri_type_decimal
			else RdfStandardIRIs.iri_type_integer
        in
	    (* spec says we store the literal as it appears in the
               file, don't canonicalise: we only convert it to check
               that it really is a number *)
	    case Real.fromString candidate_str of
		SOME i => OK (d, SOME (LITERAL {
						    value = candidate_str,
						    lang = "",
						    dtype = dtype
						}))
	      | NONE => ERROR ("expected numeric literal, found \"" ^
			       candidate_str ^ "\"")
        end
            
    (* [13] literal ::= RDFLiteral | NumericLiteral | BooleanLiteral *)
    and parse_literal d =
	case peek_ttl (d, []) of
	    C_QUOTE_SINGLE => parse_rdf_literal d
	  | C_QUOTE_DOUBLE => parse_rdf_literal d
	  | C_LETTER_T => parse_boolean_literal d
	  | C_LETTER_F => parse_boolean_literal d
	  | C_DOT => parse_numeric_literal d
	  | other => if CodepointSet.contains number (peek (d, []))
		     then parse_numeric_literal d
		     else (* not literal after all! *) ERROR "object node expected"
					
    and parse_non_literal_object d =
	case peek_ttl (d, []) of
	    C_UNDERSCORE => parse_blank_node d
	  | C_OPEN_PAREN => parse_collection d
	  | C_OPEN_SQUARE => parse_blank_node_property_list d
	  | other => parse_iri d
                               
    and parse_object d =
        if looking_at not_a_literal (d, [])
        then parse_non_literal_object d
        else parse_literal d
                               
    and parse_verb d =
	if peek_ttl (d, []) = C_OPEN_ANGLE then parse_iri d
	else parse_a_or_prefixed_name d
					    
    (* [7] predicateObjectList ::= verb objectList (';' (verb objectList)?)*
       NB we permit an empty list here; caller must reject if its rule
       demands predicateObjectList rather than predicateObjectList? *)

    and parse_predicate_object_list d =
	let
	    fun parse_object_list (d, nodes) =
                (* would be better for these if the require/discard functions just took d/source *)
		case (ignore (require_whitespace (d, []));
                      parse_object d) of
		    ERROR e => ERROR e
		  | OK (d, NONE) => ERROR "object node not found"
		  | OK (d, SOME node) =>
		    if (discard_whitespace (d, []);
                        peek_ttl (d, [])) = C_COMMA
		    then (discard (d, []);
                          parse_object_list (d, node::nodes))
		    else OK (d, rev (node::nodes))

	    fun parse_verb_object_list d =
		case parse_verb d of
		    ERROR e => ERROR ("verb IRI not found: " ^ e)
		  | OK (d, SOME (IRI iri)) =>
		    (case parse_object_list (d, []) of
			 ERROR e => ERROR e
		       | OK (d, nodes) =>
			 OK (d, map (fn n => (IRI iri, n)) nodes))
		  | OK other => ERROR "IRI expected for verb"

	    and parse_predicate_object_list' (d, volist) =
                case (discard_whitespace (d, []);
		      peek_ttl (d, [])) of
                    C_DOT => OK (d, volist)
                  | C_CLOSE_SQUARE => OK (d, volist)
                  | other => 
		    case parse_verb_object_list d of
			ERROR e => ERROR e
		      | OK (d, vos) =>
			if (discard_whitespace (d, []);
			    peek_ttl (d, [])) = C_SEMICOLON
			then (discard_greedy_ttl C_SEMICOLON (d, []);
			      parse_predicate_object_list' (d, volist @ vos))
			else OK (d, volist @ vos)
	in
	    parse_predicate_object_list' (d, [])
	end
          
    (* [10] subject ::= iri | blank *)
    and parse_subject_node d =
	case peek_ttl (d, []) of
	    C_UNDERSCORE => parse_blank_node d
	  | C_OPEN_PAREN => parse_collection d
	  | other => parse_iri d

    (* [6] triples ::= subject predicateObjectList |
                       blankNodePropertyList predicateObjectList?
       Handles the blankNodePropertyList part of that alternation *)
    and parse_blank_node_triples d =
	case parse_blank_node_property_list d of
	    ERROR e => ERROR e
          (* !!! when do we return NONE from a parser? *)
	  | OK (d, NONE) => ERROR "node expected"
	  | OK (d, SOME blank_subject) =>
	    case parse_predicate_object_list d of
		ERROR e => ERROR e
	      | OK (d, polist) =>
                OK (emit_with_subject (d, blank_subject, polist))

    (* [6] triples ::= subject predicateObjectList |
                       blankNodePropertyList predicateObjectList?
       Handles the subject part of that alternation *)
    and parse_subject_triples d =
        case parse_subject_node d of
            ERROR e => ERROR e
          | OK (d, NONE) => ERROR "node expected"
          | OK (d, SOME (LITERAL _)) => ERROR "subject may not be a literal"
          | OK (d, SOME subject_node) =>
            case parse_predicate_object_list d of
                ERROR e => ERROR e
              | OK (d, []) => ERROR "predicate missing"
              | OK (d, polist) =>
                OK (emit_with_subject (d, subject_node, polist))
                                        
    and parse_triples d =
        if peek_ttl (d, []) = C_OPEN_SQUARE
	then parse_blank_node_triples d
        else parse_subject_triples d
                                        
    (* [2] statement ::= directive | triples '.' *)
    and parse_statement d =
        case discard_whitespace (d, []) of
            ERROR e => ERROR e
          | OK (s as (d, _)) => 
            if eof s then OK (d, NONE)
            else
                if peek_ttl s = C_AT
                   orelse
                   (peek_ttl s = C_LETTER_P andalso is_prefix_tag (peek_n 6 s))
                   orelse
                   (peek_ttl s = C_LETTER_B andalso is_base_tag (peek_n 4 s))
                then parse_directive d
                else case parse_triples d of
                         ERROR e => ERROR e
               (* !!! inconsistency: this returns only one value, parse_directive returns three (with NONE) *)
                       | OK d =>
                         case require_punctuation C_DOT (d, []) of
                             ERROR e => ERROR e
                           | OK _ => OK (d, NONE)

    fun extended_error_message d e =
	let val message = e ^ " at " ^ (location (d, []))
	    val next_bit =
                case peek_n 8 (d, []) of
                    [] => peek_n 4 (d, [])
                  | p => p
	in
	    if next_bit = []
	    then message
	    else message ^ " (before \"" ^ (string_of_token next_bit) ^ "...\")"
	end

    fun parse_document (d : parse_data) : stream_value =
        let fun parse' (d : parse_data) = fn () =>
                              parse_document
                                  {
                                    source = #source d,
                                    base = #base d,
                                    prefixes = #prefixes d,
                                    blank_nodes = #blank_nodes d,
                                    new_triples = [],
                                    new_prefixes = []
                                  }
        in
            case parse_statement d of
                OK (d, _) => if eof (d, []) andalso
                                null (#new_triples d) andalso
                                null (#new_prefixes d)
                             then END_OF_STREAM
                             else PARSE_OUTPUT ({ prefixes = #new_prefixes d,
                                                  triples = #new_triples d
                                                }, parse' d)
              | ERROR e => PARSE_ERROR (extended_error_message d e)
        end

    fun split_base iristring : (token * token) =
        let fun slash_fields s = String.fields (fn x => x = #"/") iristring
            and split' i = 
                case slash_fields i of
                    [] => ("", i)
                  | [single] => (single, "")
                  | bits =>
                    ((String.concatWith "/" (rev (tl (rev bits)))) ^ "/",
                     hd (rev bits))
            val (base_iri, file_part) = split' iristring
        in
            (token_of_string base_iri, token_of_string file_part)
        end
            
    fun parse iri stream =
        parse_document {
            source = Source.from_stream stream,
            base = split_base iri,
            prefixes = TokenMap.empty,
            blank_nodes = TokenMap.empty,
            new_triples = [],
            new_prefixes = []
        }
            
end

structure TurtleParser = RdfParserFn(TurtleStreamParser)

