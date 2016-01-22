
signature PROPERTY = sig

    datatype node = datatype RdfNode.node
    type iri = Iri.t
    
    val text : Store.t * node * string -> string
    val text_list : Store.t * node * string -> string list

    val iri : Store.t * node * string -> iri option
    val iri_list : Store.t * node * string -> iri list

    val node : Store.t * node * string -> node option
    val node_list : Store.t * node * string -> node list

end
                                
        
