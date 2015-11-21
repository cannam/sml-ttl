
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
                     
    type error_state = string * source
                     
    type read_state = parse_data * source

    datatype 'a parse = ERROR of error_state | OK of 'a

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

    fun mismatch_message cp found s =
        "expected " ^ (CodepointSet.name cp) ^ ", found '" ^
        (Utf8Encode.encode_codepoint found) ^ "'"

    fun mismatch_message_ascii a found s =
        "expected '" ^ (Char.toString a) ^ "', found '" ^
        (Utf8Encode.encode_codepoint found) ^ "'"
                                                      
    fun consume_char cp (data, s) =
        if eof s then ERROR ("unexpected end of input", s)
        else let val c = read s in
                 if contains cp c then OK (data, s)
                 else ERROR (mismatch_message cp c s, s)
             end
                                                      
    fun consume_ascii a (data, s) =
        if eof s then ERROR ("unexpected end of input", s)
        else let val c = read s in
                 if c = from_ascii a then OK (data, s)
                 else ERROR (mismatch_message_ascii a c s, s)
             end

    fun discard_greedy cp (data, s) =
        if eof s then OK (data, s)
        else if contains cp (peek s)
        then discard_greedy cp (data, discard s)
        else OK (data, s)

    fun consume_to_eol (data, s) =
        if eof s then OK (data, s)
        else if contains Codepoints.eol (read s)
        then discard_greedy Codepoints.eol (data, s)
        else consume_to_eol (data, s)

    fun consume_whitespace (data, s) =
        if eof s then OK (data, s)
        else let val c = peek s in
                 if contains Codepoints.comment c then
                     case consume_to_eol (data, discard s) of
                         OK r => consume_whitespace r
                       | e => e
                 else if contains Codepoints.whitespace c then
                     consume_whitespace (data, discard s)
                 else OK (data, s)
             end

    fun discard_whitespace (data, s) =
        case consume_whitespace (data, s) of
            OK r => r 
          | _ => (data, s)

    fun consume_punctuation punct (data, s) =
        let val _ = discard_whitespace (data, s)
        in consume_ascii punct (data, s)
        end

    fun parse_iriref (data, s) = ERROR ("parse_iriref not implemented yet", s)
    fun parse_prefixed_name (data, s) = ERROR ("parse_prefixed_name not implemented yet", s)
            
    fun parse_iri_node (data, s) = 
        if looking_at_ascii #"<" s then
            parse_iriref (data, s) ~> (fn (d, s, i) => OK (d, s, IRI i))
        else
            parse_prefixed_name (data, s)
            
    fun parse_directive (data, s) = ERROR ("parse_directive not implemented yet", s)
    fun parse_blank_node (data, s) = ERROR ("parse_blank_node not implemented yet", s)
    fun parse_collection (data, s) = ERROR ("parse_collection not implemented yet", s)
    fun parse_bnode_triples (data, s) = ERROR ("parse_bnode_triples not implemented yet", s)
    fun parse_predicate_object_list (data, s) = ERROR ("parse_predicate_object_list not implemented yet", s)

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
        
    fun parse_subject_triples r =
        case parse_subject_node r of
            ERROR e => ERROR e
          | OK (d, s, LITERAL _) => ERROR ("subject may not be literal", s)
          | OK (d, s, subject_node) =>
            case parse_predicate_object_list (d, s) of
                ERROR e => ERROR e
              | OK (d, s, []) => ERROR ("predicate missing", s)
              | OK (d, s, p) => emit_with_subject (d, s, subject_node, p)
                                        
    fun parse_triples (data, s) =
        if looking_at_ascii #"[" s then parse_bnode_triples (data, s)
        else parse_subject_triples (data, s)
                                        
    (* [2] statement ::= directive | triples '.' *)
    fun parse_statement (data, s) =
        let val _ = discard_whitespace (data, s)
        in
            if eof s then OK (data, s)
            else if looking_at_ascii #"@" s then parse_directive (data, s)
            else parse_triples (data, s) ~> consume_punctuation #"."
        end
                                                 
    fun parse_document (data, s) =
        if eof s then OK (data, s)
        else
            case parse_statement (data, s) of
                OK r => parse_document r
              | e => e

    fun without_file iri =
        case String.fields (fn x => x = #"/") iri of
            [] => ""
          | bits => String.concatWith "/" (rev (tl (rev bits)))

    fun arrange_result (ERROR (e, s)) = PARSE_ERROR (e ^ " at " ^ (location s))
      | arrange_result (OK (data, s)) = PARSED {
            prefixes = StringMap.listItemsi (#prefixes data),
            triples = #triples data
        }
                                      
    fun parse_stream iri stream =
        let val source = Source.from_stream stream
        in
            arrange_result
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
                                              
                
