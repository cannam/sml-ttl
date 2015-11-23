
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

    (* The match_* functions consume and return a token, or error *)

    type token = word list

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

    (* The parse_* functions take parser data as well as source, and 
       return both (or error) *)
        
    fun parse_prefixed_name (data, s) = ERROR "parse_prefixed_name not implemented yet"
    fun parse_directive (data, s) = ERROR "parse_directive not implemented yet"
    fun parse_blank_node (data, s) = ERROR "parse_blank_node not implemented yet"
    fun parse_collection (data, s) = ERROR "parse_collection not implemented yet"
    fun parse_bnode_triples (data, s) = ERROR "parse_bnode_triples not implemented yet"
    fun parse_predicate_object_list (data, s) = ERROR "parse_predicate_object_list not implemented yet"
            
    fun parse_iriref (data, s) =
        consume_ascii #"<" s ~>
        notmatch_greedy Codepoints.iri_escaped ~>
        (fn (s, i) =>
            consume_ascii #">" s ~>
            (fn _ => OK (data, s, Utf8Encode.encode_string i)))
            
    fun parse_iri_node (data, s) = 
        if looking_at_ascii #"<" s then
            parse_iriref (data, s) ~> (fn (d, s, i) => OK (d, s, IRI i))
        else
            parse_prefixed_name (data, s)

    (* [10] subject ::= iri | blank *)
    fun parse_subject_node (data, s) =
        if looking_at_ascii #"_" s then parse_blank_node (data, s)
        else if looking_at_ascii #"(" s then parse_collection (data, s)
        else parse_iri_node (data, s)

    fun emit_with_subject (data, s, subject, polist) =
        OK (foldl (fn ((predicate, object), data) =>
                      add_triple data (subject, predicate, object))
                  data polist,
            s)
        
    fun parse_subject_triples (data, s) =
        case parse_subject_node (data, s) of
            ERROR e => ERROR e
          | OK (d, s, LITERAL _) => ERROR "subject may not be literal"
          | OK (d, s, subject_node) =>
            case parse_predicate_object_list (d, s) of
                ERROR e => ERROR e
              | OK (d, s, []) => ERROR "predicate missing"
              | OK (d, s, p) => emit_with_subject (d, s, subject_node, p)
                                        
    fun parse_triples (data, s) =
        if looking_at_ascii #"[" s then parse_bnode_triples (data, s)
        else parse_subject_triples (data, s)
                                        
    (* [2] statement ::= directive | triples '.' *)
    fun parse_statement (data, s) =
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
                                              
                
