
signature PROPERTY = sig

    type iri
    datatype node = datatype RdfNode.node
    type store
    
    val text : store * node * string -> string
    val textList : store * node * string -> string list

    val iri : store * node * string -> iri option
    val iriList : store * node * string -> iri list

    val node : store * node * string -> node option
    val nodeList : store * node * string -> node list

end

signature STORE_PROPERTY = sig

    include PROPERTY where type store = Store.t where type iri = Iri.t
                                            
end
                               
        
