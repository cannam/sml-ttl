
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

    datatype result =
             PARSE_ERROR of string |
             PARSED of {
                 prefixes : prefix list,
                 triples : triple list
             }

    val parse_string : string -> string -> result
    val parse_stream : string -> TextIO.instream -> result
    val parse_file : string -> string -> result

end

structure TurtleParser :> TURTLE_PARSER = struct

    datatype result =
             PARSE_ERROR of string |
             PARSED of {
                 prefixes : prefix list,
                 triples : triple list
             }

    (* individual tokens are read as codepoint sequences, but they're
       encoded back to utf8 strings when constructing nodes or iris *)
    type token = word list

    val token_of_string = Utf8.explode o Utf8.fromString
    val string_of_token = Utf8Encode.encode_string

    structure TokenMap = SplayMapFn (struct
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
                     
    type source = Source.t

    type parser_state = parse_data * source
                      
    datatype 'a parse = ERROR of string | OK of 'a

    fun from_ascii a = Word.fromInt (Char.ord a)
                       
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

    fun emit_with_subject (d : parse_data, s, subject, polist) =
        OK (foldl (fn ((predicate, object), data) =>
                      add_triple data (subject, predicate, object))
                  d polist,
            s)

    (* !!! this is basically the same as Option.composePartial --
    can't just use option here because I want to carry error info, but
    probably would be good to switch to the same terminology *)
	    
    fun ~> (a, b) =
        case a of
            OK result => b result
          | ERROR e => ERROR e

    infix 0 ~>

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

    open Source

    val contains = CodepointSet.contains
            
    fun looking_at cp s =
        not (eof s) andalso contains cp (peek s)

    fun looking_at_ascii a s =
        not (eof s) andalso (peek s) = from_ascii a

    fun looking_at_ascii_string a s =
	peek_n s (String.size a) = token_of_string a

    fun peek_ascii s =
	let val w = peek s in
	    if w <= 0wx7f
	    then SOME (Char.chr (Word.toInt w))
	    else NONE
	end
						  
    fun mismatch_message cp found =
        "expected " ^ (CodepointSet.name cp) ^ ", found \"" ^
        (Utf8Encode.encode_codepoint found) ^ "\""

    fun mismatch_message_ascii a found =
        "expected \"" ^ (Char.toString a) ^ "\", found \"" ^
        (Utf8Encode.encode_codepoint found) ^ "\""

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
                                   IRI (resolve_iri
                                            (data, ex @ unescape_local post)))
                  | NONE => ERROR ("unknown namespace prefix \"" ^
                                   (string_of_token pre) ^ "\"")
                
        in
            case split_at (token, from_ascii #":") of
                NONE => OK (data, s, IRI (string_of_token token))
              | SOME (pre, post) =>
                if like_pname_local_part post
                then prefix_expand' (pre, post)
                else ERROR ("malformed prefixed name \"" ^
                            (string_of_token token) ^ "\"")
        end
						  
    (* The consume_* functions require a match from the following
       input: if it matches, advance the source and return OK and the
       source (so the functions can be chained with ~> operator);
       otherwise return an error without advancing. In some cases
       (e.g. whitespace) a valid match may be empty, so only OK will
       ever actually be returned. *)
                                                  
    fun consume_char cp s =
        if eof s then ERROR "unexpected end of input"
        else let val c = read s in
                 if contains cp c then OK s
                 else ERROR (mismatch_message cp c)
             end
                                                      
    fun consume_ascii a s =
        if eof s then ERROR "unexpected end of input"
        else let val c = read s in
                 if c = from_ascii a then OK s
                 else ERROR (mismatch_message_ascii a c)
             end

    fun consume_greedy cp s =
        if eof s then OK s
        else
            if contains cp (peek s)
            then consume_greedy cp (discard s)
            else OK s

    fun consume_greedy_ascii a s =
        if eof s then OK s
        else
            if peek s = from_ascii a
            then consume_greedy_ascii a (discard s)
            else OK s
            
    fun consume_to_eol s =
        if eof s then OK s
        else
            if contains Codepoints.eol (read s)
            then consume_greedy Codepoints.eol s
            else consume_to_eol s

    fun consume_whitespace s =
        if eof s then OK s
        else
            let val c = peek s in
                if contains Codepoints.comment c then
                    case consume_to_eol (discard s) of
                        OK s => consume_whitespace s
                      | ERROR e => ERROR e
                else if contains Codepoints.whitespace_eol c then
                    consume_whitespace (discard s)
                else OK s
            end

    fun consume_required_whitespace s =
	if looking_at Codepoints.whitespace_eol s orelse
	   looking_at Codepoints.comment s
	then consume_whitespace s
	else ERROR "whitespace expected"
		
    fun consume_punctuation punct s =
        consume_whitespace s ~> consume_ascii punct

    fun have_punctuation punct s =
	(ignore (consume_whitespace s); looking_at_ascii punct s)
			   
    (* The match_* functions consume and return a token, or error *)

    (* what about non-empty matches? *)

    fun match_one cp s =
        let val c = read s in
            if CodepointSet.contains cp c
            then OK (s, c)
            else ERROR (mismatch_message cp c)
        end
            
    fun match_greedy cp s =
        let fun match_greedy' acc =
                if eof s then acc
                else
                    if contains cp (peek s)
                    then match_greedy' ((read s) :: acc)
                    else acc
        in
            OK (s, rev (match_greedy' []))
        end

    fun notmatch_greedy cp s =
        let fun notmatch_greedy' acc =
                if eof s orelse contains cp (peek s)
                then acc
                else notmatch_greedy' ((read s) :: acc)
        in
            OK (s, rev (notmatch_greedy' []))
        end

    (* Read something structured like a prefixed name. The caller is
       expected to test whether the result is properly formed depending
       on context. *)
    fun match_prefixed_name_candidate s =
	let fun match' s acc =
		case notmatch_greedy Codepoints.pname_definitely_excluded s of
		    ERROR e => ERROR e
		  | OK (s, token) =>
		    if looking_at_ascii #"." s
		    then
                        case peek_n s 2 of
                            dot::next::[] =>
                            if contains Codepoints.pname_after_dot next
                            then let val c = read s (* the dot *) in
				     match' s (acc @ token @ [c])
                                 end
                            else OK (s, acc @ token)
                          | anything_else => OK (s, acc @ token)
		    else OK (s, acc @ token)
	in
	    match' s []
	end

    fun match_percent_escape s =
        consume_ascii #"%" s ~>
            (fn s =>
                (* eval order is defined for tuples in sml (but not ocaml) *)
                case (match_one Codepoints.hex s,
                      match_one Codepoints.hex s) of
                    (* Percent escapes are *not* supposed to be
                       evaluated in an IRI in Turtle -- they should be
                       passed through unmodified *)
                    (OK (_, a), OK (s, b)) => OK (s, [from_ascii #"%", a, b])
                  | other => ERROR "expected two-digit hex value")

    fun unescape_unicode_escape s = (* !!! inconsistent name with match_percent_escape *)
        let val n = case peek_ascii s of SOME #"u" => 4 | SOME #"U" => 8 | _ =>0
            val _ = discard s
            val u = read_n s n
            val ustr = Utf8Encode.encode_string u
        in
            case Word.fromString ("0wx" ^ ustr) of
                SOME w => (s, w)
              | NONE => (s, 0wx0)
        end
            
    fun match_iriref s =
        let fun match' s acc =
                notmatch_greedy Codepoints.iri_escaped s ~>
                    (fn (s, token) =>
                        case peek_ascii s of
                            SOME #"%" =>
                            (case match_percent_escape s of
                                 ERROR e => ERROR e
                               | OK (s, pe) => match' s (acc @ token @ pe))
                          | SOME #"\\" =>
                            (ignore (discard s);
                             if looking_at Codepoints.unicode_u s
                             then let val (s, w) = unescape_unicode_escape s
                                  in if w = 0wx0
                                     then ERROR "invalid Unicode escape"
                                     else match' s (acc @ token @ [w])
                                  end
                             else ERROR "expected Unicode escape")
                          | other => OK (s, acc @ token))
        in
            consume_ascii #"<" s ~>
                (fn s => match' s [] ~>
                     (fn (s, token) =>
                         consume_ascii #">" s ~>
                                       (fn s => OK (s, token))))
        end

    fun match_prefixed_name_namespace s =
	case match_prefixed_name_candidate s of
	    ERROR e => ERROR e
	  | OK (s, []) => ERROR "malformed prefix"
	  | OK (s, [w]) => if w = from_ascii #":"
			   then OK (s, [])
			   else ERROR "expected \":\" at end of prefix"
	  | OK (s, all) => 
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
		    then OK (s, token)
		    else ERROR ("malformed prefix \"" ^ (string_of_token token) ^ "\"")
		else ERROR "expected \":\" at end of prefix"
	    end

    datatype quote = NO_QUOTE | SHORT_STRING of char | LONG_STRING of char

    fun have_three s =
	case peek_n s 3 of [a,b,c] => a = b andalso b = c
			 | anything_else => false

    fun match_quote s =
	let fun short_or_long s q =
		if have_three s
		then LONG_STRING q
		else SHORT_STRING q
	in		
	    case peek_ascii s of
		SOME #"\"" => short_or_long s #"\""
	      | SOME #"'"  => short_or_long s #"'"
	      | other => NO_QUOTE
	end

    fun match_long_string_body s q =
        consume_ascii q s ~> consume_ascii q ~> consume_ascii q ~>
        (fn s =>
                      let val cp = if q = #"'"
		         then Codepoints.string_single_excluded
		         else Codepoints.string_double_excluded
                fun match' s acc =
                    case notmatch_greedy cp s of
                        ERROR e => ERROR e
                      | OK (s, body) => (*!!! handle escapes *)
                        let val n = read s in
                            if n = from_ascii q andalso peek_n s 2 = [n, n]
                            then
                                consume_ascii q s ~> consume_ascii q ~>
                                              (fn s => OK (s, acc @ body))
                            else
                                match' s (acc @ body @ [n])
                        end
            in
                match' s [] ~> (fn (s, body) => OK (s, string_of_token body))
            end)

    fun unescape_string_escape s =
        let val e = read s in
            case Codepoints.CharMap.find (Codepoints.string_escape_map, e) of
                SOME w => (s, w)
              | NONE => (s, e)
        end
            
    fun match_short_string_body s q =
	let val cp = if q = #"'"
		     then Codepoints.string_single_excluded
		     else Codepoints.string_double_excluded
            fun match' s acc =
	        case notmatch_greedy cp s of
		    ERROR e => ERROR e
	          | OK (s, body) =>
                    if looking_at_ascii #"\\" s
                    then (ignore (discard s);
                          if looking_at Codepoints.string_escape s
                          then let val (s, w) = unescape_string_escape s
                               in match' s (acc @ body @ [w])
                               end
                          else if looking_at Codepoints.unicode_u s
                          then let val (s, w) = unescape_unicode_escape s
                               in if w = 0wx0
                                  then ERROR "invalid Unicode escape"
                                  else match' s (acc @ body @ [w])
                               end
                          else ERROR "expected escape sequence")
                    else OK (s, string_of_token (acc @ body))
        in
	    consume_ascii q s ~>
                          (fn s =>
                              match' s [] ~>
                                     (fn (s, str) =>
                                         consume_ascii q s ~>
                                                       (fn s => OK (s, str))))
        end
		     
    fun match_string_body s =
	case match_quote s of
	    NO_QUOTE => ERROR "expected quotation mark"
	  | SHORT_STRING q => match_short_string_body s q
	  | LONG_STRING q => match_long_string_body s q

    fun match_language_tag s =
        (*!!! this is a "match_" but datatype has to be a "parse_", why? *)
        let fun parse' s =
                case match_greedy Codepoints.alpha s of
                    ERROR e => []
                  | OK (s, token) =>
                    case peek_ascii s of
                        SOME #"-" => token @ [read s] @ (parse' s)
                      | other => token
        in
            consume_ascii #"@" s ~>
                (fn s => case parse' s of
                             [] => ERROR "non-empty language tag expected"
                           | tag => OK tag)
        end
            
    (* The parse_* functions take parser data as well as source, and 
       return both, as well as the parsed node or whatever (or error) *)

    fun parse_base (data, s) = 
	consume_required_whitespace s ~>
        match_iriref ~>
	(fn (s, token) =>
            let val base = token_of_string (resolve_iri (data, token))
            in
                OK ({ file_iri = base,
                      base_iri = base,
                      triples = #triples data,
                      prefixes = #prefixes data,
                      blank_nodes = #blank_nodes data
                    }, s)
            end)
                
    and parse_prefix (data, s) =
	consume_required_whitespace s ~>
        match_prefixed_name_namespace ~>
	(fn (s, prefix) =>
	    consume_required_whitespace s ~>
                (fn s => case match_iriref s of
			     ERROR e => ERROR e
			   | OK (s, iri) => OK (add_prefix data (prefix, iri), s)))
	
    and parse_sparql_base (data, s) = ERROR "parse_sparql_base not implemented yet"
    and parse_sparql_prefix (data, s) = ERROR "parse_sparql_prefix not implemented yet"
		     
    and parse_directive (data, s) =
	case peek_ascii s of
	    SOME #"@" =>
	    (ignore (discard s) ;
	     match_greedy Codepoints.alpha s ~>
			  (fn (s, token) =>
			      (* !!! v ugly *)
			      case string_of_token token of
				  "prefix" => (case parse_prefix (data, s) of
						  ERROR e => ERROR e
						| OK (data, s) => consume_punctuation #"." s ~> (fn s => OK (data, s)))
				| "base" => (case parse_base (data, s) of
						 ERROR e => ERROR e
					       | OK (data, s) => consume_punctuation #"." s ~> (fn s => OK (data, s)))
				| other => ERROR ("expected \"prefix\" or \"base\" after @, not \"" ^ other ^ "\"")))
	  | SOME #"B" => parse_sparql_prefix (data, s)
	  | SOME #"b" => parse_sparql_prefix (data, s)
	  | SOME #"P" => parse_sparql_prefix (data, s)
	  | SOME #"p" => parse_sparql_prefix (data, s)
	  | other => ERROR "expected @prefix, @base, PREFIX, or BASE"
	
    and parse_collection (data, s) = ERROR "parse_collection not implemented yet"

    and parse_blank_node (data, s) =
	consume_ascii #"_" s ~> consume_ascii #":" ~>
            match_prefixed_name_candidate ~>
            (fn (s, candidate) =>
		(* !!! check that we match the blank node pattern. that is:
                   one initial_bnode_char
                   zero or more pname_char_or_dot
                   if there were any of those, then finally one pname_char *)
		case blank_node_for (data, candidate) of
		    (data, node) => OK (data, s, node))
	    
    and parse_prefixed_name (data, s) =
	case match_prefixed_name_candidate s of
	    ERROR e => ERROR e
	  | OK (s, token) =>
            (* We can't tell the difference, until we get here,
               between a prefixed name and the bare literals true
               or false *)
            if token = true_token then OK (data, s,  new_boolean_literal true)
            else if token = false_token then OK (data, s, new_boolean_literal false)
            else prefix_expand (data, s, token)
  
    and parse_iriref (data, s) =
	match_iriref s ~> (fn (s, iri) => OK (data, s, IRI (resolve_iri (data, iri))))
            
    and parse_iri (data, s) = 
        if looking_at_ascii #"<" s
        then parse_iriref (data, s)
        else parse_prefixed_name (data, s)

    and parse_a_or_prefixed_name (data, s) =
	case match_prefixed_name_candidate s of
	    ERROR e => ERROR e
	  | OK (s, token) =>
	    if token = [ from_ascii #"a" ]
	    then OK (data, s, IRI RdfTypes.iri_rdf_type)
	    else prefix_expand (data, s, token)

    and parse_blank_node_property_list (data, s) =
	consume_punctuation #"[" s ~>
	    (fn s =>
		case parse_predicate_object_list (data, s) of
                    ERROR e => ERROR e
		  | OK (d, s, p) =>  (* p may legitimately be empty *)
		    let val blank_node = new_blank_node () in
			emit_with_subject (d, s, blank_node, p) ~>
					  (fn (d, s) =>
					      consume_punctuation #"]" s ~>
						  (fn s => OK (d, s, blank_node)))
		    end)

    (* [133s] BooleanLiteral ::= 'true' | 'false' *)
    and parse_boolean_literal (data, s) =
	if looking_at_ascii_string "true" s
	then OK (data, s, new_boolean_literal true)
	else if looking_at_ascii_string "false" s
	then OK (data, s, new_boolean_literal false)
	else ERROR "expected \"true\" or \"false\""

    and parse_datatype (data, s) =
        consume_ascii #"^" s ~> consume_ascii #"^" ~>
                      (fn s => parse_iri (data, s))
                   
    and parse_rdf_literal (data, s) =
	case match_string_body s of
	    ERROR e => ERROR e
	  | OK (s, body) =>
            case peek_ascii s of
                SOME #"@" => (case match_language_tag s of
                                  ERROR e => ERROR e
                                | OK tag =>
                                  OK (data, s, LITERAL {
                                          value = body,
                                          lang = string_of_token tag,
                                          dtype = ""
                             }))
              | SOME #"^" => (case parse_datatype (data, s) of
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
	case peek_ascii s of
	    SOME #"'"  => parse_rdf_literal (data, s)
	  | SOME #"\"" => parse_rdf_literal (data, s)
	  | SOME #"t"  => parse_boolean_literal (data, s)
	  | SOME #"f"  => parse_boolean_literal (data, s)
	  | SOME #"."  => parse_numeric_literal (data, s)
	  | other => if CodepointSet.contains Codepoints.number (peek s)
		     then parse_numeric_literal (data, s)
		     else (* not literal after all! *) ERROR "object node expected"
					
    and parse_non_literal_object (data, s) =
	case peek_ascii s of
	    SOME #"_" => parse_blank_node (data, s)
	  | SOME #"(" => parse_collection (data, s)
	  | SOME #"[" => parse_blank_node_property_list (data, s)
	  | other => parse_iri (data, s)
                               
    and parse_object (data, s) =
        if looking_at Codepoints.not_a_literal s
        then parse_non_literal_object (data, s)
        else parse_literal (data, s)
                               
    and parse_verb (data, s) =
	if looking_at_ascii #"<" s then parse_iri (data, s)
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
		    if have_punctuation #"," s
		    then (ignore (consume_ascii #"," s);
			  parse_object_list (d, s) (node::acc))
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
		if have_punctuation #"." s orelse have_punctuation #"]" s
		then OK (d, s, acc) (* empty list, or list ending with ";" *)
		else
		    case parse_verb_object_list (d, s) of
			ERROR e => ERROR e
		      | OK (d, s, vol) =>
			if have_punctuation #";" s
			then (ignore (consume_greedy_ascii #";" s);
			      parse_predicate_object_list' (d, s) (acc @ vol))
			else OK (d, s, acc @ vol)
	in
	    parse_predicate_object_list' (data, s) []
	end
          
    (* [10] subject ::= iri | blank *)
    and parse_subject_node (data, s) =
	case peek_ascii s of
	    SOME #"_" => parse_blank_node (data, s)
	  | SOME #"(" => parse_collection (data, s)
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
        if looking_at_ascii #"[" s
	then parse_blank_node_triples (data, s)
        else parse_subject_triples (data, s)
                                        
    (* [2] statement ::= directive | triples '.' *)
    and parse_statement (data, s) =
        consume_whitespace s ~>
        (fn s =>
            if eof s then OK (data, s)
            else
                if looking_at_ascii #"@" s
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
                                              
                
