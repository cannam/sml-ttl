		    
signature RDF_TRIPLE = sig

    type iri
    datatype node = datatype RdfNode.node
    type triple = node * node * node

    (*!!! why is this here? just so as to be able to print it? why isn't it associated with the prefix table? *)
    type prefix = string * iri  (* prefix, expansion *)

    (* Compare two triples rapidly and consistently within a given
       runtime, by comparing the nodes in order from left to
       right. See RDF_NODE.compare for notes on the limitations of
       node comparison. *)
    val compare : triple * triple -> order
                                
    (* Debug streaming, not guaranteed to match a standard format *)
    val string_of_prefix : prefix -> string

    (* Debug streaming, not guaranteed to match a standard format *)
    val string_of_triple : triple -> string

    (* Debug streaming, not guaranteed to match a standard format *)
    val string_of_triples : triple list -> string

end

