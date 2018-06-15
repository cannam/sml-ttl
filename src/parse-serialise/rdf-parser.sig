
(** Parser that reads from a text stream and produces a complete
    parsed set of prefixes and triples "in one go".

    Note that an RDF_PARSER parses a single format; different formats
    would have different parser structures, and distinguishing between
    them must be handled at a higher level. So there is no means to
    tell the parser what format to parse.
 *)
signature RDF_PARSER = sig

    type prefix = Prefix.prefix
    type triple = RdfTriple.triple
    type base_iri = BaseIri.t

    datatype parsed =
             (** Error produced during parsing, e.g. malformed syntax *)
             PARSE_ERROR of string |
             (** Successful parse *)
             PARSED of {
                 base : base_iri,
                 prefixes : prefix list,
                 triples : triple list
             }

    (** Parse an entire document from a stream in one go. Does not close
        the input stream after parsing. *)
    val parse : base_iri * TextIO.instream -> parsed

end

(** Parser that reads from a text stream and emits prefixes and
    triples as it sees them. Only some RDF serialisation formats can
    be parsed in this way. When the parse function returns some
    output, it is accompanied by a thunk that can be called to
    continue parsing, until the end of the stream is reached.

    Note that any given RDF_INCREMENTAL_PARSER implementation parses a
    single format; different formats would have different parser
    structures, and distinguishing between them must be handled at a
    higher level. So there is no means to tell the parser what format
    to parse.
*)
signature RDF_INCREMENTAL_PARSER = sig
    
    type prefix = Prefix.prefix
    type triple = RdfTriple.triple
    type base_iri = BaseIri.t

    datatype stream_value =
             (** End of stream reached following successful parse *)
             END_OF_STREAM |
             (** Error produced during parsing, e.g. malformed syntax *)
             PARSE_ERROR of string |
             (** Some parsed output is available *)
             PARSE_OUTPUT of {
                 base : base_iri,
                 prefixes : prefix list,
                 triples : triple list                 
             } * (unit -> stream_value)

    (** Process the stream and return a batch of parsed elements. Does
        not close the input stream after parsing; this function should
        normally be called repeatedly on the same input stream until
        it returns END_OF_STREAM. *)
    val parse : base_iri * TextIO.instream -> stream_value
                     
end
