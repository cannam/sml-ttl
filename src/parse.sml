
datatype node =
         IRI of string |
         BLANK of int |
         LITERAL of {
             value : string,
             dtype : string,
             lang  : string
         }

type triple = node * node * node

type prefix = string * string

signature TURTLE_PARSER = sig

    datatype parsed =
             PARSE_ERROR of string |
             PARSED of {
                 prefixes : prefix list,
                 triples : triple list
             }

    val parse_string : string -> string -> parsed
    val parse_stream : string -> TextIO.instream -> parsed
    val parse_file : string -> string -> parsed

end

structure TurtleParser :> TURTLE_PARSER = struct

    datatype parsed =
             PARSE_ERROR of string |
             PARSED of {
                 prefixes : prefix list,
                 triples : triple list
             }

    (* individual tokens are read as codepoint sequences, but they're
       encoded back to utf8 strings when constructing nodes or iris *)
    type token = word list

    datatype significant_char = datatype Codepoints.turtle_significant_char

    val token_of_string = Utf8.explode o Utf8.fromString
    val string_of_token = Utf8.toString o Utf8.implode

    structure TokenMap = RedBlackMapFn (struct
                                         type ord_key = token
                                         val compare = List.collate Word.compare
                                         end)

    type parse_data = {
        file_iri : token,
        base_iri : token,
        triples : triple list,
        prefixes : token TokenMap.map,      (* prefix -> expansion *)
        blank_nodes : int TokenMap.map      (* token -> blank node id *)
    }

    (* todo: subsume source into parse_data ? *)
                          
    type source = Source.t

    type match_state = parse_data * source * token
    type parse_state = parse_data * source * node option
                      
    datatype 'a result = ERROR of string | OK of 'a

    type match_result = match_state result
    type parse_result = parse_state result

    fun from_ascii a = Word.fromInt (Char.ord a)

    (* !!! pretty sure each of these is called only in one place, so
           maybe should just write them inline *)
                                    
    fun add_triple (d : parse_data) (t : triple) =
        { file_iri = #file_iri d,
          base_iri = #base_iri d,
          triples = t :: #triples d,
          prefixes = #prefixes d,
          blank_nodes = #blank_nodes d }
                     
    fun add_prefix (d : parse_data) (p, e) =
        { file_iri = #file_iri d,
          base_iri = #base_iri d,
          triples = #triples d,
          prefixes = TokenMap.insert (#prefixes d, p, e),
          blank_nodes = #blank_nodes d }
                     
    fun add_bnode (d : parse_data) (b, id) =
        { file_iri = #file_iri d,
          base_iri = #base_iri d,
          triples = #triples d,
          prefixes = #prefixes d,
          blank_nodes = TokenMap.insert (#blank_nodes d, b, id) }

    fun emit_with_subject (d : parse_data, subject, polist) =
        foldl (fn ((predicate, object), data) =>
                  add_triple data (subject, predicate, object))
              d polist

    fun composePartial (f, g) =
        fn a => 
           case f a of
               OK result => g result
             | ERROR e => ERROR e

    fun sequence [] = (fn x => OK x)
      | sequence funcs =
        (* e.g. [ f, g, h ] -> composePartial (h, composePartial (g, f)) *)
        let val rf = rev funcs
        in foldl composePartial (hd rf) (tl rf)
        end
                                
    fun new_boolean_literal b =
        LITERAL {
            value = if b then "true" else "false",
            lang = "",
            dtype = RdfTypes.iri_type_boolean
        }
    val true_token = token_of_string "true"
    val false_token = token_of_string "false"

    val bnode_counter = ref 0
    fun new_blank_node () =
	let val id = !bnode_counter in
	    bnode_counter := id + 1;
	    BLANK id
	end
				      
    fun blank_node_for (d: parse_data, token) =
	case TokenMap.find (#blank_nodes d, token) of
	    SOME id => (d, BLANK id)
	  | NONE =>
	    case new_blank_node () of
		BLANK id => (add_bnode d (token, id), BLANK id)
              | _ => raise Fail "new_blank_node returned non-blank node"

    fun peek (_,s,_) = Source.peek s
    fun peek_n n (_,s,_) = Source.peek_n n s
    fun read (_,s,_) = Source.read s
    fun read_n n (_,s,_) = Source.read_n n s
    fun location (_,s,_) = Source.location s
    fun eof (_,s,_) = Source.eof s

    fun looking_at cps st =
        not (eof st) andalso CodepointSet.contains cps (peek st)

    fun looking_at_token tok st =
	peek_n (List.length tok) st = tok

    fun peek_ttl s =
        let val w = peek s in
            case Codepoints.CharMap.find (Codepoints.significant_char_map, w) of
                SOME significant => significant
              | NONE => C_NOTHING_INTERESTING
        end
						  
    fun mismatch_message cps found =
        "expected " ^ (CodepointSet.name cps) ^ ", found \"" ^
        (string_of_token [found]) ^ "\""

    fun split_at (lst, elt) =
        let fun split' ([], acc) = NONE
              | split' (x::xs, acc) = if x = elt then SOME (rev acc, xs)
                                      else split' (xs, x::acc)
        in
            split' (lst, [])
        end

    fun resolve_iri (data, token) =
        let val bi = #base_iri data
            val fi = #file_iri data
            fun like_absolute_iri [] = false
              | like_absolute_iri (first::rest) = 
                if CodepointSet.contains Codepoints.alpha first
                then like_absolute_iri rest
                else if first = from_ascii #":"
                then case rest of
                         s1::s2::_ => s1 = s2 andalso s1 = from_ascii #"/"
                       | _ => false
                else false
        in
	    string_of_token
                (case token of
                     [] => bi
                   | first::rest =>
                     if first = from_ascii #"#"
                     then fi @ token
                     else if first = from_ascii #"/" then bi @ rest
                     else if like_absolute_iri token then token
                     else bi @ token)
        end

    fun unescape_local token = token (*!!!*)
            
    fun prefix_expand (data, s, token) =
        let fun like_pname_local_part token =
                true (*!!! todo! *)
                (* first check post matches rePNLocal:
               one char that is either rePNCharsU, 0-9, :, or rePlx
               zero+ chars that are rePNChars, ., :, rePlx
               maybe one char that is rePNChars, :, rePlx
               where rePlx is rePercent (i.e. % + 2 hex chars) or
                   rePNLocalEsc (which is backslash + pname_local_escapable) *)

            fun prefix_expand' (pre, post) =
                (* We don't check the prefix for well-formedness,
                   because if it isn't well-formed, it won't match
                   anything in our namespace map -- because we did
                   check for well-formedness when making the map *)
                case TokenMap.find (#prefixes data, pre) of
                    SOME ex => OK (data, s,
                                   SOME (IRI (resolve_iri
						  (data, ex @ unescape_local post))))
                  | NONE => ERROR ("unknown namespace prefix \"" ^
                                   (string_of_token pre) ^ "\"")
                
        in
            case split_at (token, from_ascii #":") of
                NONE => OK (data, s, SOME (IRI (string_of_token token)))
              | SOME (pre, post) =>
                if like_pname_local_part post
                then prefix_expand' (pre, post)
                else ERROR ("malformed prefixed name \"" ^
                            (string_of_token token) ^ "\"")
        end

    (* 
     types of reader function: discard, require, match, parse

     discard -> reads 0 or more of something, throws it away, never fails

     require -> reads 1 or more of something, throws it away, fails if not present

     match -> reads something, appends it to token part of parse_state
              tuple, fails if not matched

     parse -> reads something, possibly stashes it in parse_data, fails if not
              matched
     *)

    fun discard (d, source, t) = (Source.discard source; (d, source, t))

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
            if CodepointSet.contains Codepoints.eol (read s)
            then discard_greedy Codepoints.eol s
            else discard_to_eol s

    fun discard_whitespace s =
        if eof s then OK s
        else
            let val c = peek s in
                if CodepointSet.contains Codepoints.comment c then
                    case discard_to_eol (discard s) of
                        OK s => discard_whitespace s
                      | ERROR e => ERROR e
                else if CodepointSet.contains Codepoints.whitespace_eol c then
                    discard_whitespace (discard s)
                else OK s
            end

    fun require cps s =
        if eof s then ERROR "unexpected end of input"
        else let val c = read s in
                 if CodepointSet.contains cps c then OK s
                 else ERROR (mismatch_message cps c)
             end

    fun require_ttl c s =
        if eof s then ERROR "unexpected end of input"
        else if Codepoints.CharMap.find (Codepoints.significant_char_map,
                                         peek s) = SOME c
        then OK (discard s)
        else ERROR ("unexpected character \"" ^ (string_of_token [peek s]) ^ "\"")

    fun require_whitespace s =
	if looking_at Codepoints.whitespace_eol s orelse
	   looking_at Codepoints.comment s
	then discard_whitespace s
	else ERROR "whitespace expected"
		
    fun require_punctuation c s =
        sequence [ discard_whitespace, require_ttl c ] s

    fun match cps (s as (d, source, tok)) : match_result =
        let val c = read s in
            if CodepointSet.contains cps c
            then OK (d, source, tok @ [c])
            else ERROR (mismatch_message cps c)
        end

    (* May return an empty token *)
    fun match_token cps (s as (d, source, tok)) : match_result =
        let fun match' acc =
                if eof s then acc
                else
                    if CodepointSet.contains cps (peek s)
                    then match' ((read s) :: acc)
                    else acc
        in
            OK (d, source, rev (match' (rev tok)))
        end

    (* May return an empty token *)
    fun match_token_excl cps (s as (d, source, tok)) : match_result =
        let fun match' acc =
                if eof s orelse CodepointSet.contains cps (peek s)
                then acc
                else match' ((read s) :: acc)
        in
            OK (d, source, rev (match' (rev tok)))
        end

    (* Read something structured like a prefixed name. The caller is
       expected to test whether the result is properly formed depending
       on context. *)
    fun match_prefixed_name_candidate s : match_result =
	let fun match' s acc =
		case match_token_excl Codepoints.pname_definitely_excluded s of
		    ERROR e => ERROR e
		  | OK (s as (d, source, token)) =>
		    if peek_ttl s = C_DOT
		    then
                        case peek_n 2 s of
                            dot::next::[] =>
                            if CodepointSet.contains Codepoints.pname_after_dot next
                            then let val c = read s (* the dot *) in
				     match' s (acc @ token @ [c])
                                 end
                            else OK (d, source, acc @ token)
                          | anything_else => OK (d, source, acc @ token)
		    else OK (d, source, acc @ token)
	in
	    match' s []
	end

    fun match_percent_escape s : match_result =
        (* Percent escapes are *not* supposed to be evaluated in an
           IRI -- they should be passed through unmodified *)
        sequence [ require_ttl C_PERCENT,
                   match Codepoints.hex,
                   match Codepoints.hex ] s
        
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
                case match_token_excl Codepoints.iri_escaped s of
                    ERROR e => ERROR e
                  | OK (s as (d, source, token)) => 
                    case peek_ttl s of
                        C_PERCENT =>
                        (case match_percent_escape s of
                             ERROR e => ERROR e
                           | OK (d, source, pe) => match' (d, source, token @ pe))
                      | C_BACKSLASH =>
                        (discard s;
                         if looking_at Codepoints.unicode_u s
                         then let val (s, w) = unescape_unicode_escape s
                              in if w = 0wx0
                                 then ERROR "invalid Unicode escape"
                                 else match' (d, source, token @ [w])
                              end
                         else ERROR "expected Unicode escape")
                      | other => OK (d, source, token)
        in
            sequence [ require_ttl C_OPEN_ANGLE,
                       match',
                       require_ttl C_CLOSE_ANGLE ] s
        end

    fun match_prefixed_name_namespace s : match_result =
	case match_prefixed_name_candidate s of
	    ERROR e => ERROR e
	  | OK (d, source, []) => ERROR "malformed prefix"
	  | OK (d, source, [w]) => if w = from_ascii #":"
			           then OK (d, source, [])
			           else ERROR "expected \":\" at end of prefix"
	  | OK (d, source, all) => 
	    let val first = hd all
		val r = rev all
		val colon = hd r
		val last = hd (tl r)
		val token = rev (tl r)
	    in
		if colon = from_ascii #":"
		then
		    if CodepointSet.contains Codepoints.base_pname_char first
		       andalso List.all (CodepointSet.contains
					     Codepoints.pname_char_or_dot) token
		       andalso CodepointSet.contains Codepoints.pname_char last
		    then OK (d, source, token)
		    else ERROR ("malformed prefix \"" ^ (string_of_token token) ^ "\"")
		else ERROR "expected \":\" at end of prefix"
	    end

    datatype quote = NO_QUOTE |
                     SHORT_STRING of Codepoints.turtle_significant_char |
                     LONG_STRING of Codepoints.turtle_significant_char

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

    fun quote_codepoint q =
        if q = C_QUOTE_SINGLE
	then Codepoints.string_single_excluded
	else Codepoints.string_double_excluded
                     
    fun match_long_string_body s q : match_result =
        let fun match' s =
                case match_token_excl (quote_codepoint q) s of
                    ERROR e => ERROR e
                  | OK s => (*!!! todo: handle escapes *)
                    if have_three s then OK s
                    else match' s
        in
            sequence [
                require_ttl q, require_ttl q, require_ttl q,
                match',
                require_ttl q, require_ttl q, require_ttl q
            ] s
        end
                 
    fun unescape_string_escape s =
        let val e = read s in
            case Codepoints.CharMap.find (Codepoints.string_escape_map, e) of
                SOME w => (s, w)
              | NONE => (s, e)
        end
            
    fun match_short_string_body s q : match_result =
	let fun match' s =
                case match_token_excl (quote_codepoint q) s of
		    ERROR e => ERROR e
	          | OK (s as (d, source, body)) =>
                    if peek_ttl s = C_BACKSLASH
                    then (discard s;
                          if looking_at Codepoints.string_escape s
                          then let val (s, w) = unescape_string_escape s
                               in match' (d, source, body @ [w])
                               end
                          else if looking_at Codepoints.unicode_u s
                          then let val (s, w) = unescape_unicode_escape s
                               in if w = 0wx0
                                  then ERROR "invalid Unicode escape"
                                  else match' (d, source, body @ [w])
                               end
                          else ERROR "expected escape sequence")
                    else OK s
        in
            sequence [
                require_ttl q,
                match',
                require_ttl q
            ] s
        end
		     
    fun match_string_body s : match_result =
	case match_quote s of
	    NO_QUOTE => ERROR "expected quotation mark"
	  | SHORT_STRING q => match_short_string_body s q
	  | LONG_STRING q => match_long_string_body s q

    fun match_language_tag s : match_result =
        let fun match' s =
                case match_token Codepoints.alpha s of
                    ERROR e => ERROR e
                  | OK (s as (d, source, token)) =>
                    case peek_ttl s of
                        C_DASH => match' (d, source, token @ [read s])
                      | other => OK s
        in
            sequence [
                require_ttl C_AT,
                match',
                fn (d, source, []) => ERROR "non-empty language tag expected"
                  | s => OK s
            ] s
        end
            
    (* The parse_* functions take parser data as well as source, and 
       return both, as well as the parsed node or whatever (or error) *)

    fun parse_base (d, source) : parse_result =
        case sequence [
	        require_whitespace,
                match_iriref
            ] (d, source, []) of
            ERROR e => ERROR e
          | OK (s as (d, source, token)) => 
            let val base = token_of_string (resolve_iri (d, token))
            in
                OK ({ file_iri = base,
                      base_iri = base,
                      triples = #triples d,
                      prefixes = #prefixes d,
                      blank_nodes = #blank_nodes d
                    }, source, NONE)
            end
                 
    and parse_prefix (d, source) : parse_result =
        case sequence [
	        require_whitespace,
                match_prefixed_name_namespace,
                require_whitespace
            ] (d, source, []) of
            ERROR e => ERROR e
          | OK (d, source, prefix) =>
            case match_iriref (d, source, []) of
                ERROR e => ERROR e
              | OK (d, source, iri) =>
	        OK (add_prefix d (prefix, iri), source, NONE)
	
    and parse_sparql_base (d, source) : parse_result =
        case match_prefixed_name_candidate (d, source, []) of
            ERROR e => ERROR e
          | OK (d, source, token) =>
            if token = token_of_string "base" orelse
               token = token_of_string "BASE" then
                parse_base (d, source)
            else
                ERROR "expected \"BASE\""
	
    and parse_sparql_prefix (d, source) : parse_result =
        case match_prefixed_name_candidate (d, source, []) of
            ERROR e => ERROR e
         | OK (d, source, token) => 
           if token = token_of_string "prefix" orelse
              token = token_of_string "PREFIX" then
               parse_prefix (d, source)
           else
               ERROR "expected \"PREFIX\""
        
    and parse_directive (d, source) : parse_result =
        let val s = (d, source, []) in
	    case peek_ttl s of
	        C_LETTER_B => parse_sparql_prefix (d, source)
	      | C_LETTER_P => parse_sparql_prefix (d, source)
	      | C_AT =>
                (case sequence [
                         require_ttl C_AT,
                         match_token Codepoints.alpha,
                         require_punctuation C_DOT
                     ] s of
                     ERROR e => ERROR e
                   | OK (d, source, token) => 
                     case string_of_token token of
                         "prefix" => parse_prefix (d, source)
                       | "base" => parse_base (d, source)
		       | other => ERROR ("expected \"prefix\" or \"base\" after @, not \"" ^ other ^ "\""))
	      | other => ERROR "expected @prefix, @base, PREFIX, or BASE"
        end
	                   
    and parse_collection (d, source) : parse_result = ERROR "parse_collection not implemented yet"

    and parse_blank_node (d, source) : parse_result =
	case sequence [
		require_ttl C_UNDERSCORE,
		require_ttl C_COLON,
		match_prefixed_name_candidate
	    ] (d, source, []) of
	    ERROR e => ERROR e
	  | OK (d, source, candidate) => 
		(* !!! check that we match the blank node pattern. that is:
                   one initial_bnode_char
                   zero or more pname_char_or_dot
                   if there were any of those, then finally one pname_char *)
	    case blank_node_for (d, candidate) of
		(d, node) => OK (d, source, SOME node)
	    
    and parse_prefixed_name (d, source) : parse_result =
	case match_prefixed_name_candidate (d, source, []) of
	    ERROR e => ERROR e
	  | OK (d, source, token) =>
            (* We can't tell the difference, until we get here,
               between a prefixed name and the bare literals true
               or false *)
            if token = true_token
	    then OK (d, source, SOME (new_boolean_literal true))
            else if token = false_token
	    then OK (d, source, SOME (new_boolean_literal false))
            else prefix_expand (d, source, token)
  
    and parse_iriref (d, source) : parse_result=
	case match_iriref (d, source, []) of
	    ERROR e => ERROR e
	  | OK (d, source, token) =>
	    OK (d, source, SOME (IRI (resolve_iri (d, token))))
            
    and parse_iri (d, source) : parse_result = 
        if peek_ttl (d, source, []) = C_OPEN_ANGLE
        then parse_iriref (d, source)
        else parse_prefixed_name (d, source)

    and parse_a_or_prefixed_name (d, source) : parse_result =
	case match_prefixed_name_candidate (d, source, []) of
	    ERROR e => ERROR e
	  | OK (d, source, token) =>
	    if token = [ from_ascii #"a" ]
	    then OK (d, source, SOME (IRI RdfTypes.iri_rdf_type))
	    else prefix_expand (d, source, token)

    and parse_blank_node_property_list (d, source) : parse_result =
	case require_ttl C_OPEN_SQUARE (d, source, []) of
	    ERROR e => ERROR e
	  | OK (d, source, _) => 
	    case parse_predicate_object_list (d, source) of
                ERROR e => ERROR e
	      | OK (d, source, p) =>  (* p may legitimately be empty *)
		let val blank_node = new_blank_node ()
		    val d = emit_with_subject (d, blank_node, p)
		in
		    case require_ttl C_CLOSE_SQUARE (d, source, []) of
			ERROR e => ERROR e
		      | OK (d, source, _) => OK (d, source, SOME blank_node)
		end

    (* [133s] BooleanLiteral ::= 'true' | 'false' *)
    and parse_boolean_literal (d, source) : parse_result =
	if looking_at_token true_token (d, source, [])
	then OK (d, source, SOME (new_boolean_literal true))
	else if looking_at_token false_token (d, source, [])
	then OK (d, source, SOME (new_boolean_literal false))
	else ERROR "expected \"true\" or \"false\""

    and parse_datatype (d, source) : parse_result =
	case sequence [
		require_ttl C_CARET,
		require_ttl C_CARET
	    ] (d, source, []) of
	    ERROR e => ERROR e
	  | OK (d, source, _) => parse_iri (d, source)
                   
    and parse_rdf_literal (data, s) =
	case match_string_body s of
	    ERROR e => ERROR e
	  | OK (s, body) =>
            case peek_ttl s of
                C_AT => (case match_language_tag s of
                             ERROR e => ERROR e
                           | OK tag =>
                             OK (data, s, LITERAL {
                                     value = body,
                                     lang = string_of_token tag,
                                     dtype = ""
                        }))
              | C_CARET => (case parse_datatype (data, s) of
                                ERROR e => ERROR e
                              | OK (data, s, IRI tag) =>
                                OK (data, s, LITERAL {
                                        value = body,
                                        lang = "",
                                        dtype = tag
                                   })
                              | other => ERROR "internal error")
              | other => OK (data, s, LITERAL {
		                 value = body,
		                 lang = "",
		                 dtype = ""
		            })
	
    and parse_numeric_literal (data, s) =
        let val point = from_ascii #"."
            val candidate =
                case match_greedy Codepoints.number s of
                    ERROR e => []
                  | OK (s, n0) =>
                    case peek_n s 2 of
                        [a,b] => if a = point andalso
                                    CodepointSet.contains
                                        Codepoints.number_after_point b
                                 then
                                     case match_greedy Codepoints.number
                                                       (discard s) of
                                         OK (s, n1) => (n0 @ [point] @ n1)
                                       | ERROR e => []
                                 else n0
                      | _ => n0
	    val candidate_str = string_of_token candidate
	    val contains_e =
		(List.find (CodepointSet.contains Codepoints.exponent) candidate)
		<> NONE
	    val contains_dot =
		(List.find (fn c => c = from_ascii #".") candidate) <> NONE
        in
	    (* spec says we store the literal as it appears in the
               file, don't canonicalise: we only convert it to check
               that it really is a number *)
	    case Real.fromString candidate_str of
		SOME i => OK (data, s, LITERAL {
				  value = candidate_str,
				  lang = "",
				  dtype = if contains_e
					  then RdfTypes.iri_type_double
					  else if contains_dot
					  then RdfTypes.iri_type_decimal
					  else RdfTypes.iri_type_integer
			      })
	      | NONE => ERROR "numeric literal expected"
        end
                 
    (* [13] literal ::= RDFLiteral | NumericLiteral | BooleanLiteral *)
    and parse_literal (data, s) =
	case peek_ttl s of
	    C_QUOTE_SINGLE => parse_rdf_literal (data, s)
	  | C_QUOTE_DOUBLE => parse_rdf_literal (data, s)
	  | C_LETTER_T => parse_boolean_literal (data, s)
	  | C_LETTER_F => parse_boolean_literal (data, s)
	  | C_DOT => parse_numeric_literal (data, s)
	  | other => if CodepointSet.contains Codepoints.number (peek s)
		     then parse_numeric_literal (data, s)
		     else (* not literal after all! *) ERROR "object node expected"
					
    and parse_non_literal_object (data, s) =
	case peek_ttl s of
	    C_UNDERSCORE => parse_blank_node (data, s)
	  | C_OPEN_PAREN => parse_collection (data, s)
	  | C_OPEN_SQUARE => parse_blank_node_property_list (data, s)
	  | other => parse_iri (data, s)
                               
    and parse_object (data, s) =
        if looking_at Codepoints.not_a_literal s
        then parse_non_literal_object (data, s)
        else parse_literal (data, s)
                               
    and parse_verb (data, s) =
	if peek_ttl s = C_OPEN_ANGLE then parse_iri (data, s)
	else parse_a_or_prefixed_name (data, s)
					    
    (* [7] predicateObjectList ::= verb objectList (';' (verb objectList)?)*
       NB we permit an empty list here; caller must reject if its rule
       demands predicateObjectList rather than predicateObjectList? *)

    and parse_predicate_object_list (data, s) =
	let
	    fun parse_object_list (d, s) acc =
		case (ignore (consume_whitespace s); parse_object (d, s)) of
		    ERROR e => ERROR e
		  | OK (d, s, node) =>
		    if peek_ttl s = C_COMMA
		    then (discard s; parse_object_list (d, s) (node::acc))
		    else OK (d, s, rev (node::acc))

	    fun parse_verb_object_list (d, s) =
		case parse_verb (d, s) of
		    ERROR e => ERROR ("verb IRI not found: " ^ e)
		  | OK (d, s, IRI iri) =>
		    (case parse_object_list (d, s) [] of
			 ERROR e => ERROR e
		       | OK (d, s, nodes) =>
			 OK (d, s, map (fn n => (IRI iri, n)) nodes))
		  | OK other => ERROR "IRI expected for verb"

	    and parse_predicate_object_list' (d, s) acc =
                case peek_ttl s of
                    C_DOT => OK (d, s, acc)
                  | C_CLOSE_SQUARE => OK (d, s, acc)
                  | other => 
		    case parse_verb_object_list (d, s) of
			ERROR e => ERROR e
		      | OK (d, s, vol) =>
			if peek_ttl s = C_SEMICOLON
			then (ignore (consume_greedy_identical s);
			      parse_predicate_object_list' (d, s) (acc @ vol))
			else OK (d, s, acc @ vol)
	in
	    parse_predicate_object_list' (data, s) []
	end
          
    (* [10] subject ::= iri | blank *)
    and parse_subject_node (data, s) =
	case peek_ttl s of
	    C_UNDERSCORE => parse_blank_node (data, s)
	  | C_OPEN_PAREN => parse_collection (data, s)
	  | other => parse_iri (data, s)

    (* [6] triples ::= subject predicateObjectList |
                       blankNodePropertyList predicateObjectList?

       Handles the blankNodePropertyList part of that alternation *)
    and parse_blank_node_triples (data, s) =
	case parse_blank_node_property_list (data, s) of
	    ERROR e => ERROR e
	  | OK (d, s, blank_subject) =>
	    case parse_predicate_object_list (d, s) of
		ERROR e => ERROR e
	      | OK (d, s, p) => emit_with_subject (d, s, blank_subject, p)

    (* [6] triples ::= subject predicateObjectList |
                       blankNodePropertyList predicateObjectList?
 
       Handles the subject part of that alternation *)
    and parse_subject_triples (data, s) =
        case parse_subject_node (data, s) of
            ERROR e => ERROR e
          | OK (d, s, LITERAL _) => ERROR "subject may not be a literal"
          | OK (d, s, subject_node) =>
            case parse_predicate_object_list (d, s) of
                ERROR e => ERROR e
              | OK (d, s, []) => ERROR "predicate missing"
              | OK (d, s, p) => emit_with_subject (d, s, subject_node, p)
                                        
    and parse_triples (data, s) =
        if peek_ttl s = C_OPEN_SQUARE
	then parse_blank_node_triples (data, s)
        else parse_subject_triples (data, s)
                                        
    (* [2] statement ::= directive | triples '.' *)
    and parse_statement (data, s) =
        consume_whitespace s ~>
        (fn s =>
            if eof s then OK (data, s)
            else
                if peek_ttl s = C_AT
                then parse_directive (data, s)
                else parse_triples (data, s) ~>
                     (fn r => (consume_punctuation #"." s; OK r)))
                                                 
    fun parse_document (data, s) =
        if eof s then OK (data, s)
        else
            case parse_statement (data, s) of
                OK r => parse_document r
              | ERROR e => ERROR e

    fun without_file iri =
        case String.fields (fn x => x = #"/") iri of
            [] => ""
          | bits => String.concatWith "/" (rev (tl (rev bits)))

    fun arrange_result s (ERROR e) =
	let val message = e ^ " at " ^ (location s)
	    val next_bit = case peek_n s 8 of [] => peek_n s 4 | p => p
	in
	    if next_bit = []
	    then PARSE_ERROR message
	    else PARSE_ERROR (message ^ " (before \"" ^
			      (string_of_token next_bit) ^ "...\")")
	end
      | arrange_result s (OK (data, _)) =
	PARSED {
            prefixes = map (fn (a,b) => (string_of_token a, string_of_token b))
                           (TokenMap.listItemsi (#prefixes data)),
            triples = #triples data
        }
                                      
    fun parse_stream iri stream =
        let val source = Source.from_stream stream
            val data = {
                file_iri = token_of_string iri,
                base_iri = token_of_string (without_file iri),
                triples = [],
                prefixes = TokenMap.empty,
                blank_nodes = TokenMap.empty
            }
            val parsed = parse_document (data, source)
        in
            print "done\n";
            arrange_result source parsed
        end

    fun parse_string iri string =
        let val stream = TextIO.openString string
            val result = parse_stream iri stream
        in
            TextIO.closeIn stream;
            result
        end

    fun parse_file iri filename =
        let val stream = TextIO.openIn filename
            val result = parse_stream iri stream
        in
            TextIO.closeIn stream;
            result
        end
                                                 
end
                                              
                
