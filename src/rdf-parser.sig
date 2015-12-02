			 
signature RDF_PARSER = sig

    type prefix
    type triple
    
    datatype parsed =
             PARSE_ERROR of string |
             PARSED of {
                 prefixes : prefix list,
                 triples : triple list
             }

    type base_iri = string
(*
    val parse_stream_async : (parsed * 'a -> 'a) -> base_iri -> TextIO.instream -> 'a -> 'a
    val parse_string_async : (parsed * 'a -> 'a) -> base_iri -> string -> 'a -> 'a
    val parse_file_async   : (parsed * 'a -> 'a) -> base_iri -> string -> 'a -> 'a
*)			   
    val parse_stream : base_iri -> TextIO.instream -> parsed
    val parse_string : base_iri -> string -> parsed
    val parse_file   : base_iri -> string -> parsed

end
