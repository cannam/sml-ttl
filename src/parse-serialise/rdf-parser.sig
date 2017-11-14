
(** Parser that reads from a text stream and produces a complete
    parsed set of prefixes and triples "in one go".

    Note that an RDF_PARSER parses a single format; different formats
    would have different parser structures, and distinguishing between
    them must be handled at a higher level. So there is no means to
    tell the parser what format to parse.
 *)
signature RDF_PARSER = sig

    type prefix = RdfTriple.prefix
    type triple = RdfTriple.triple
    
    datatype parsed =
             (** Error produced during parsing, e.g. malformed syntax *)
             PARSE_ERROR of string |
             (** Successful parse *)
             PARSED of {
                 prefixes : prefix list,
                 triples : triple list
             }

    type base_iri = string
                
    (** Parse an entire document from a stream in one go. Does not close
        the input stream after parsing. *)
    val parse : base_iri -> TextIO.instream -> parsed

end

(** Parser that reads from a text stream and emits prefixes and
    triples as it sees them. Only some RDF serialisation formats can
    be parsed in this way. When the parse function returns some
    output, it is accompanied by a thunk that can be invoked to 

    Note that an RDF_INCREMENTAL_PARSER parses a single format;
    different formats would have different parser structures, and
    distinguishing between them must be handled at a higher level. So
    there is no means to tell the parser what format to parse.
*)
signature RDF_INCREMENTAL_PARSER = sig
    
    type prefix = RdfTriple.prefix
    type triple = RdfTriple.triple

    datatype stream_value =
             (** End of stream reached following successful parse *)
             END_OF_STREAM |
             (** Error produced during parsing, e.g. malformed syntax *)
             PARSE_ERROR of string |
             (** Some parsed output is available *)
             PARSE_OUTPUT of {
                 prefixes : prefix list,
                 triples : triple list                 
             } * (unit -> stream_value)

    (*!!! surely should be an Iri.t ? *)
    (*!!! what if the file defines a base iri, as a ttl file can? maybe we don't even need this here, but only in export/serialise *)
    type base_iri = string
                     
    (** Process the stream and return a batch of parsed elements. Does
        not close the input stream after parsing; this function should
        normally be called repeatedly on the same input stream until
        it returns END_OF_STREAM. *)
    val parse : base_iri -> TextIO.instream -> stream_value
                     
end
