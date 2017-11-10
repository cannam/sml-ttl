
(*!!! make file + sig naming consistent with property *)

signature COLLECTION_GATHERER = sig

    type matcher
    datatype node = datatype RdfNode.node
    type triple = node * node * node
    
    (* Return true if the given node looks like a collection node,
       i.e. a metanode, a node with rdf:rest property. *)
    val is_collection_node : matcher * node -> bool

    (* Given a collection node and a matcher that can provide matching
       nodes, return the first collection node (i.e. the node with
       rdf:rest property) of that collection. Assumes the collection
       is well-formed and may return spurious results if it is not *)
    val start_of_collection : matcher * node -> node

    (* Given a collection node and a matcher that can provide matching
       nodes, return all rdf:first and rdf:rest triples that comprise
       the collection, in collection order. *)
    val triples_of_collection : matcher * node -> triple list

    (* Given a collection node and a matcher that can provide matching
       nodes, return all nodes contained in the collection, in
       order. *)
    val nodes_of_collection : matcher * node -> node list
                                                  
end

signature COLLECTION_EXPANDER = sig

    datatype node = datatype RdfNode.node
    type triple = node * node * node

    (* Given a list of nodes, return a list of triples comprising the
       RDF collection of those nodes. *)
    val collection_of_nodes : node list -> triple list
end
                                    
