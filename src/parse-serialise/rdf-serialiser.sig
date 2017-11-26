
(** Serialiser that writes RDF triples to a text stream.
*)
signature RDF_SERIALISER = sig

    type triple = RdfTriple.triple
    type t

    (** Add a series of triples to the output. *)
    val serialise : t * triple list -> t

    (** Write any pending triples and clean up. Does not close any
        resources (e.g. output stream) that may have been passed to
        the constructor in an already-open state. *)
    val finish : t -> unit
                          
end

(** Serialiser that writes RDF triples to a text stream one at a time,
    without any interesting features like abbreviation or collections
    support.
*)
signature RDF_INCREMENTAL_SERIALISER = sig

    include RDF_SERIALISER

    (** Construct a serialiser to an output text stream. Stream must
        be open for writing, and it will remain open after
        serialisation has finished: it is the caller's responsibility
        to close it. *)
    val new : TextIO.outstream -> t

end

(** Serialiser that writes RDF triples to a text stream with
    abbreviations and collections support. May batch up the triples
    and emit them all when finish is called.
*)
signature RDF_ABBREVIATING_SERIALISER = sig

    include RDF_SERIALISER
    
    type prefixTable
    type matcher

    type baseIri = BaseIri.t

    (** Construct a serialiser using a given set of prefixes and a
        matcher for collection gathering, writing to an output text
        stream. Stream must be open for writing, and it will remain
        open after serialisation has finished: it is the caller's
        responsibility to close it. *)
    val new : baseIri * prefixTable * matcher -> TextIO.outstream -> t

end
			       
