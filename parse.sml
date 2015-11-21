
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
    
    val parse_stream : TextIO.instream -> result
    val parse_file : string -> result

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
                     
    type text = word list

    type error_state = string * text
                     
    type read_state = parse_data * text
                     
    datatype partial_result =
             ERROR of error_state |
             OK of read_state
	         
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
                    
    fun ~> a b =
        case a of
            OK result => b result
          | ERROR e => ERROR e

    infix 0 ~>

    fun looking_at cp [] = false
      | looking_at cp (c::cs) = CodepointSet.contains cp c

    fun expected_error cp found =
        "expected " ^ (CodepointSet.name cp) ^ ", found '" ^
        (Utf8Encode.encode_codepoint found) ^ "'"
                                                      
    fun consume_char cp (data, []) = ERROR ("unexpected end of input", [])
      | consume_char cp (data, c::cs) = if CodepointSet.contains cp c
                                        then OK (data, cs)
                                        else ERROR (expected_error cp c, c::cs)

    fun discard_greedy cp (data, []) = OK (data, [])
      | discard_greedy cp (data, c::cs) = if CodepointSet.contains cp c
                                          then discard_greedy cp (data, cs)
                                          else OK (data, c::cs)
                                                   
    fun consume_to_eol (data, []) = OK (data, [])
      | consume_to_eol (data, c::cs) = 
        if CodepointSet.contains Codepoints.eol c
        then discard_greedy Codepoints.eol (data, cs)
        else consume_to_eol (data, cs)
                        
    fun consume_whitespace (data, []) = OK (data, [])
      | consume_whitespace (data, c::cs) =
        if CodepointSet.contains Codepoints.comment c
        then case consume_to_eol (data, cs) of OK r => consume_whitespace r
        else
            if CodepointSet.contains Codepoints.whitespace c
            then consume_whitespace (data, cs)
            else OK (data, c::cs)

    fun discard_whitespace p =
        case consume_whitespace p of OK r => r
                             
    fun fold_line f acc line =
        case line of
            [] => SOME acc
          | codepoint::rest =>
            case f (codepoint, acc) of
                NONE => NONE
              | SOME acc' => fold_line f acc' rest

    fun fold_stream f acc stream =
        case TextIO.inputLine stream of
            NONE => SOME acc
          | SOME line =>
            case f (line, acc) of
                NONE => NONE
              | SOME acc' => fold_stream f acc' stream
                                                    
    fun parse_stream stream =
        PARSE_ERROR "not implemented yet"

    fun parse_file filename =
        let val stream = TextIO.openIn filename in
            let val result = parse_stream stream in
                TextIO.closeIn stream;
                result
            end
        end
            
end
                                              
                
