
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

    type accumulation = {
        file_iri : string,
        base_iri : string,
        triples : triple list,
        prefixes : string StringMap.map, (* prefix -> expansion *)
        bnodes : int StringMap.map       (* string -> blank node id *)
    }
                     
    type text = word list

    type error_state = string * text
                     
    type read_state = accumulation * text
                     
    datatype partial_result =
             ERROR of error_state |
             OK of read_state
                     
    fun add_triple (s : accumulation) (t : triple) =
        { file_iri = #file_iri s,
          base_iri = #base_iri s,
          triples = t :: #triples s,
          prefixes = #prefixes s,
          bnodes = #bnodes s }
                     
    fun add_prefix (s : accumulation) ((p, e) : prefix) =
        { file_iri = #file_iri s,
          base_iri = #base_iri s,
          triples = #triples s,
          prefixes = StringMap.insert (#prefixes s, p, e),
          bnodes = #bnodes s }
                     
    fun add_bnode (s : accumulation) (b, id) =
        { file_iri = #file_iri s,
          base_iri = #base_iri s,
          triples = #triples s,
          prefixes = #prefixes s,
          bnodes = StringMap.insert (#bnodes s, b, id) }

    fun looking_at cp [] = false
      | looking_at cp (c::cs) = CodepointSet.contains cp c

    fun expected_error_text cp found =
        "expected " ^ (CodepointSet.name cp) ^ ", found '" ^
        (Utf8Encode.encode_codepoint found) ^ "'"
                                                      
    fun consume cp (data, []) = ERROR ("unexpected end of input", [])
      | consume cp (data, c::cs) = if CodepointSet.contains cp c
                                   then OK (data, cs)
                                   else ERROR (expected_error_text cp c, c::cs)
                   
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
                                              
                
