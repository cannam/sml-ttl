
signature RDF_NODE = sig

    eqtype iri

    type literal = {
	value : string,
	dtype : iri,
	lang  : string
    }

    datatype node =
             IRI of iri |
             BLANK of int |
             LITERAL of literal

    val newBlankNode : unit -> node

    (* Compare two nodes rapidly and consistently within a given
       runtime. The ordering is guaranteed to place IRI nodes first,
       blank nodes next, and literal nodes last. Within node type no
       particular ordering is assured, e.g. IRIs may not be
       lexicographically ordered but instead ordered on some internal
       key. For this reason, orderings are not "persistent" across
       runtimes (two IRIs could compare differently in separate runs
       of a program). *)
    val compare : node * node -> order

    (* Debug streaming, not guaranteed to match a standard format *)
    val stringOfNode : node -> string
				     
end
