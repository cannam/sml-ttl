
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
                     
    datatype partial_result =
             ERROR of error_state |
             OK of read_state

    fun from_ascii a = Word.fromInt (Char.ord a)
                       
    fun add_triple (s : parse_data) (t : triple) =
        { file_iri = #file_iri s,
          base_iri = #base_iri s,
          triples = t :: #triples s,
          prefixes = #prefixes s,
          bnodes = #bnodes s }
                     
    fun add_prefix (s : parse_data) ((p, e) : prefix) =
        { file_iri = #file_iri s,
          base_iri = #base_iri s,
          triples = #triples s,
          prefixes = StringMap.insert (#prefixes s, p, e),
          bnodes = #bnodes s }
                     
    fun add_bnode (s : parse_data) (b, id) =
        { file_iri = #file_iri s,
          base_iri = #base_iri s,
          triples = #triples s,
          prefixes = #prefixes s,
          bnodes = StringMap.insert (#bnodes s, b, id) }
                    
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
                 else if contains Codepoints.whitespace c then
                     consume_whitespace (data, discard s)
                 else OK (data, s)
             end

    fun discard_whitespace (data, s) =
        case consume_whitespace (data, s) of OK r => r

    fun consume_punctuation punct (data, s) =
        let val _ = discard_whitespace (data, s)
        in consume_ascii punct (data, s)
        end

    fun parse_directive (data, s) = ERROR ("not implemented yet", s)
    fun parse_triples (data, s) = ERROR ("not implemented yet", s)
            
    (* [2] statement ::= directive | triples '.' *)
    fun parse_statement (data, s) =
        let val _ = discard_whitespace (data, s)
        in
            if eof s then OK (data, s)
            else if looking_at_ascii #"@" s then
                parse_directive (data, s)
            else
                parse_triples (data, s) ~> consume_punctuation #"."
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
                    
    fun parse_stream iri stream =
        let val source = Source.from_stream stream
        in
            parse_document ({
                               file_iri = iri,
                               base_iri = without_file iri,
                               triples = [],
                               prefixes = StringMap.empty,
                               bnodes = StringMap.empty
                           }, source)
        end

    fun parse_file iri filename =
        let val stream = TextIO.openIn filename
            val result = parse_stream iri stream
        in
            TextIO.closeIn stream;
            result
        end
                                                 
end
                                              
                
