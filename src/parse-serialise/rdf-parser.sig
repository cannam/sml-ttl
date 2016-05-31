
signature RDF_PARSER_BASE = sig
    
    type prefix = RdfTriple.prefix
    type triple = RdfTriple.triple
    
    datatype parsed =
             PARSE_ERROR of string |
             PARSED of {
                 prefixes : prefix list,
                 triples : triple list
             }

    type base_iri = string

end

signature RDF_PARSER = sig

    (* RDF_PARSER is the signature for a parser that reads from a text
       stream and produces a complete parsed set of prefixes and
       triples.

       This signature is for parsers that parse a single format, and
       so don't need to be told what format to parse. *)

    include RDF_PARSER_BASE
                
    (* does not close the input stream after parsing *)
    val parse : base_iri -> TextIO.instream -> parsed

end

signature RDF_STREAM_PARSER = sig

    (* RDF_PARSER is the signature for a parser that reads from a text
       stream and emits prefixes and triples as it sees them. Only
       some RDF serialisation formats can be parsed in this way.

       This signature is for parsers that parse a single format, and
       so don't need to be told what format to parse. *)
    
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
                     
    (* does not close the input stream after parsing *)
    val parse : base_iri -> TextIO.instream -> stream_value
                     
end
                                  
signature RDF_FILE_PARSER = sig
                           
    (* RDF_FILE_PARSER is the signature for a parser that reads from a
       file and produces a complete parsed set of prefixes and
       triples. It uses file metadata, e.g. suffix, to determine the
       format to be parsed, and will probably work by guessing and
       delegating to an appropriate RDF_PARSER internally. *)

    include RDF_PARSER_BASE

    val parse_file : base_iri -> string -> parsed
                
end
