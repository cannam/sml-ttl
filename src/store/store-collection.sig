
(*!!! make file + sig naming consistent with property *)

signature COLLECTION = sig

    type store
    datatype node = datatype RdfNode.node
    type triple = node * node * node
    
    (* Return true if the given node looks like a collection node,
       i.e. a metanode, a node with rdf:rest property. *)
    val is_collection_node : store * node -> bool

    (* Given a collection node and the store that contains it, return
       the first collection node (i.e. the node with rdf:rest
       property) of that collection. Assumes the collection is
       well-formed and may return spurious results if it is not *)
    val start_of_collection : store * node -> node

    (* Given a collection node and the store that contains it, return
       all rdf:first and rdf:rest triples that comprise the collection,
       in collection order. *)
    val triples_of_collection : store * node -> triple list

    (* Given a collection node and the store that contains it, return
       all nodes contained in the collection, in order. *)
    val nodes_of_collection : store * node -> node list

end

signature STORE_COLLECTION = sig

    include COLLECTION where type store = Store.t
                                              
end
