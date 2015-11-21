
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

signature SOURCE = sig

    type t

    val open_file : string -> t
    val take_stream : TextIO.instream -> t
    val close : t -> unit
    val peek : t -> word
    val read : t -> word
    val discard : t -> t
    val location : t -> int * int
    val location_string : t -> string
    val eof : t -> bool

end
                         
structure Source :> SOURCE = struct

    val nl = Word.fromInt (Char.ord #"\n")

    type t = {
        stream : TextIO.instream,
        line : word list ref,
        lineno : int ref,
        colno : int ref
    }

    fun load_line r =
        (case TextIO.inputLine (#stream r) of
             NONE =>
             (#line r) := []
           | SOME str =>
             ((#line r) := Utf8.explode (Utf8.fromString str);
              (#lineno r) := !(#lineno r) + 1;
              (#colno r) := 1);
         r)

    fun take_stream str =
        load_line { stream = str, line = ref [], lineno = ref 0, colno = ref 0 }
                 
    fun open_file filename =
        take_stream (TextIO.openIn filename)

    fun close r =
        TextIO.closeIn (#stream r)

    fun peek r =
        case !(#line r) of
            first::rest => first
          | [] => nl

    fun read r =
        case !(#line r) of
            first::next::rest =>
            ((#line r) := next::rest;
             (#colno r) := !(#colno r) + 1;
             first)
          | first::[] => (load_line r; first)
          | [] => nl

    fun discard r =
        let val _ = read r in r end
                      
    fun location r =
        (!(#lineno r), !(#colno r))

    fun location_string r =
        "line " ^ (Int.toString (!(#lineno r))) ^
        ", column " ^ (Int.toString (!(#colno r)))

    fun eof r =
        (!(#line r) = [])
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

    open Source

    val contains = CodepointSet.contains
            
    fun looking_at cp s =
        not (eof s) andalso contains cp (peek s)

    fun mismatch_message cp found s =
        "expected " ^ (CodepointSet.name cp) ^ ", found '" ^
        (Utf8Encode.encode_codepoint found) ^ "' at " ^
        (Source.location_string s)
                                                      
    fun consume_char cp (data, s) =
        if eof s then ERROR ("unexpected end of input", s)
        else let val c = read s in
                 if contains cp c then OK (data, s)
                 else ERROR (mismatch_message cp c s, s)
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

    fun discard_whitespace p =
        case consume_whitespace p of OK r => r

    fun parse_stream str = PARSE_ERROR "blah"

    fun parse_file filename = PARSE_ERROR "blah"
                                                 
end
                                              
                
