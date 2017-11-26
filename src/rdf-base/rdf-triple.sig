		    
signature RDF_TRIPLE = sig

    type iri
    datatype node = datatype RdfNode.node
    type triple = node * node * node

    (* Compare two triples rapidly and consistently within a given
       runtime, by comparing the nodes in order from left to
       right. See RDF_NODE.compare for notes on the limitations of
       node comparison. *)
    val compare : triple * triple -> order

    (* Debug streaming, not guaranteed to match a standard format *)
    val stringOfTriple : triple -> string

    (* Debug streaming, not guaranteed to match a standard format *)
    val stringOfTriples : triple list -> string

end

