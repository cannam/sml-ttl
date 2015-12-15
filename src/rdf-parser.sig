			 
signature RDF_PARSER = sig

    type prefix = RdfTriple.prefix
    type triple = RdfTriple.triple
    
    datatype parsed =
             PARSE_ERROR of string |
             PARSED of {
                 prefixes : prefix list,
                 triples : triple list
             }

    type base_iri = string

    val parse : base_iri -> TextIO.instream -> parsed (* does not close the stream *)

end

signature RDF_STREAM_PARSER = sig

    type prefix = RdfTriple.prefix
    type triple = RdfTriple.triple

    datatype stream_value =
             END_OF_STREAM |
             PARSE_ERROR of string |
             PARSE_OUTPUT of {
                 prefixes : prefix list,
                 triples : triple list                 
             } * (unit -> stream_value)

    type base_iri = string
                     
    val parse : base_iri -> TextIO.instream -> stream_value (* does not close the stream *)
                     
end
                                  
