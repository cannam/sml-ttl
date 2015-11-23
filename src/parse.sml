
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
                             
    structure StringMap = SplayMapFn (struct
                                       type ord_key = string
                                       val compare = String.compare
                                       end)

    type parse_data = {
        file_iri : string,
        base_iri : string,
        triples : triple list,
        prefixes : string StringMap.map, (* prefix -> expansion *)
        bnodes : int StringMap.map       (* string -> blank node id *)
    }
                     
    type source = Source.t

    type parser_state = parse_data * source
                      
    datatype 'a parse = ERROR of string | OK of 'a

    (* individual tokens are read as codepoint sequences, but they're
       encoded back to utf8 strings when constructing nodes or iris *)
    type token = word list

    fun from_ascii a = Word.fromInt (Char.ord a)
                       
    fun add_triple (d : parse_data) (t : triple) =
        { file_iri = #file_iri d,
          base_iri = #base_iri d,
          triples = t :: #triples d,
          prefixes = #prefixes d,
          bnodes = #bnodes d }
                     
    fun add_prefix (d : parse_data) ((p, e) : prefix) =
        { file_iri = #file_iri d,
          base_iri = #base_iri d,
          triples = #triples d,
          prefixes = StringMap.insert (#prefixes d, p, e),
          bnodes = #bnodes d }
                     
    fun add_bnode (d : parse_data) (b, id) =
        { file_iri = #file_iri d,
          base_iri = #base_iri d,
          triples = #triples d,
          prefixes = #prefixes d,
          bnodes = StringMap.insert (#bnodes d, b, id) }

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

    open Source

    val contains = CodepointSet.contains
            
    fun looking_at cp s =
        not (eof s) andalso contains cp (peek s)

    fun looking_at_ascii a s =
        not (eof s) andalso (peek s) = from_ascii a

    fun mismatch_message cp found =
        "expected " ^ (CodepointSet.name cp) ^ ", found '" ^
        (Utf8Encode.encode_codepoint found) ^ "'"

    fun mismatch_message_ascii a found =
        "expected '" ^ (Char.toString a) ^ "', found '" ^
        (Utf8Encode.encode_codepoint found) ^ "'"

    fun split_at (lst, elt) =
        let fun split' ([], acc) = NONE
              | split' (x::xs, acc) = if x = elt then SOME (rev acc, xs)
                                      else split' (xs, x::acc)
        in
            split' (lst, [])
        end
                                                  
    fun prefix_expand (data, s, token) = 
        case split_at (token, from_ascii #":") of
            NONE => OK (data, s, IRI (Utf8Encode.encode_string token))
          | SOME (pre, post) =>
            (* first check post matches rePNLocal:
               one char that is either rePNCharsU, 0-9, :, or rePlx
               zero+ chars that are rePNChars, ., :, rePlx
               maybe one char that is rePNChars, :, rePlx
               where rePlx is rePercent (i.e. % + 2 hex chars) or
                   rePNLocalEsc (which is backslash + pname_local_escapable) *)
            ERROR "difficult bit not yet done"
						  
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
                else if contains Codepoints.whitespace c then
                    consume_whitespace (discard s)
                else OK s
            end

    fun consume_punctuation punct s =
        consume_whitespace s ~> consume_ascii punct

    fun have_punctuation punct s =
	(ignore (consume_whitespace s); looking_at_ascii punct s)
			   
    (* The match_* functions consume and return a token, or error *)

    (* what about non-empty matches? *)
                      
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
	let fun match_prefixed_name_candidate' s acc =
		case notmatch_greedy Codepoints.pname_definitely_excluded s of
		    ERROR e => ERROR e
		  | OK (s, token) =>
		    if looking_at_ascii #"." s
		    then
                        case peek_n s 2 of
                            dot::next::[] =>
                            if contains Codepoints.pname_after_dot next
                            then let val c = read s (* the dot *) in
                                     match_prefixed_name_candidate'
                                         s (acc @ token @ [c])
                                 end
                            else OK (acc @ token)
                          | anything_else => OK (acc @ token)
		    else OK (acc @ token)
	in
	    match_prefixed_name_candidate' s []
	end
	    
    (* The parse_* functions take parser data as well as source, and 
       return both, as well as the parsed node or whatever (or error) *)
        
    and parse_directive (data, s) = ERROR "parse_directive not implemented yet"
    and parse_blank_node (data, s) = ERROR "parse_blank_node not implemented yet"
    and parse_collection (data, s) = ERROR "parse_collection not implemented yet"
    and parse_bnode_triples (data, s) = ERROR "parse_bnode_triples not implemented yet"
    and parse_literal (data, s) = ERROR "parse_literal not implemented yet"

    and parse_prefixed_name (data, s) =
        let fun boolean v =
                LITERAL {
                    value = v,
                    lang = "",
                    dtype = RdfTypes.iri_type_boolean
                }
            val t = Utf8.explode (Utf8.fromString "true")
            val f = Utf8.explode (Utf8.fromString "false")
        in
	    case match_prefixed_name_candidate s of
	        ERROR e => ERROR e
	      | OK token =>
                (* We can't tell the difference, until we get here,
                   between a prefixed name and the bare literals true
                   or false *)
                if token = t then OK (data, s,  boolean "true")
                else if token = f then OK (data, s, boolean "false")
                else prefix_expand (data, s, token)
        end
  
    and parse_iriref (data, s) =
        consume_ascii #"<" s ~>
        notmatch_greedy Codepoints.iri_escaped ~>
        (fn (s, i) =>
            consume_ascii #">" s ~>
            (fn _ => OK (data, s, IRI (Utf8Encode.encode_string i))))
            
    and parse_iri (data, s) = 
        if looking_at_ascii #"<" s
        then parse_iriref (data, s)
        else parse_prefixed_name (data, s)

    and parse_a_or_prefixed_name (data, s) =
	case match_prefixed_name_candidate s of
	    ERROR e => ERROR e
	  | OK token =>
	    if token = [ from_ascii #"a" ]
	    then OK (data, s, IRI RdfTypes.iri_rdf_type)
	    else prefix_expand (data, s, token)

    and parse_blank_node_property_list (data, s) = ERROR "parse_blank_node_property_list not implemented yet"
                               
    and parse_non_literal_object (data, s) =
        if looking_at_ascii #"_" s
        then parse_blank_node (data, s)
        else if looking_at_ascii #"(" s
        then parse_collection (data, s)
        else if looking_at_ascii #"[" s
        then parse_blank_node_property_list (data, s)
        else parse_iri (data, s)
                               
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
	    fun parse_object_list s =
		case (ignore (consume_whitespace s); parse_object (data, s)) of
		    ERROR e => ERROR e
		  | OK (data, s, node) =>
		    if have_punctuation #"," s
		    then (ignore (consume_ascii #"," s);
			  case parse_object_list s of
			      ERROR e => ERROR e
			    | OK nodes => OK (node::nodes))
		    else OK [node]

	    fun parse_verb_object_list s =
		case parse_verb (data, s) of
		    ERROR e => ERROR ("verb IRI not found: " ^ e)
		  | OK (data, s, IRI iri) =>
		    (case parse_object_list s of
			 ERROR e => ERROR e
		       | OK nodes => OK (map (fn n => (IRI iri, n)) nodes))
		  | OK other => ERROR "IRI expected for verb"

	    and parse_predicate_object_list' s acc =
		if have_punctuation #"." s orelse have_punctuation #"]" s
		then OK (data, s, acc) (* empty list, or list ending with ";" *)
		else
		    case parse_verb_object_list s of
			ERROR e => ERROR e
		      | OK vol =>
			if have_punctuation #";" s
			then (ignore (consume_greedy_ascii #";" s);
			      parse_predicate_object_list' s (acc @ vol))
			else OK (data, s, acc @ vol)
	in
	    parse_predicate_object_list' s []
	end
          
    (* [10] subject ::= iri | blank *)
    and parse_subject_node (data, s) =
        if looking_at_ascii #"_" s then parse_blank_node (data, s)
        else if looking_at_ascii #"(" s then parse_collection (data, s)
        else parse_iri (data, s)
        
    and parse_subject_triples (data, s) =
        case parse_subject_node (data, s) of
            ERROR e => ERROR e
          | OK (d, s, LITERAL _) => ERROR "subject may not be literal"
          | OK (d, s, subject_node) =>
            case parse_predicate_object_list (d, s) of
                ERROR e => ERROR e
              | OK (d, s, []) => ERROR "predicate missing"
              | OK (d, s, p) => emit_with_subject (d, s, subject_node, p)
                                        
    and parse_triples (data, s) =
        if looking_at_ascii #"[" s then parse_bnode_triples (data, s)
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

    fun arrange_result s (ERROR e) = PARSE_ERROR (e ^ " at " ^ (location s))
      | arrange_result s (OK (data, _)) = PARSED {
            prefixes = StringMap.listItemsi (#prefixes data),
            triples = #triples data
        }
                                      
    fun parse_stream iri stream =
        let val source = Source.from_stream stream
        in
            arrange_result source
                (parse_document ({ file_iri = iri,
                                   base_iri = without_file iri,
                                   triples = [],
                                   prefixes = StringMap.empty,
                                   bnodes = StringMap.empty
                                }, source))
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
                                              
                
