
signature STORE_COLLECTION = sig

    type store
    datatype node = datatype RdfNode.node
    type triple = node * node * node
    
    (* Return true if the given node looks like a collection node,
       i.e. a metanode, a node with rdf:first and rdf:next property. *)
    val is_collection_node : store * node -> bool

    (* Given a collection node and the store that contains it, return
       the first collection node (i.e. the node with rdf:first and
       rdf:next property) of that collection. Assumes the collection
       is well-formed and may return spurious results if it is not *)
    val start_of_collection : store * node -> node

    (* Given a collection node and the store that contains it, return
       all rdf:first and rdf:rest triples that comprise the collection,
       in collection order. *)
    val triples_of_collection : store * node -> triple list

    (* Given a collection node and the store that contains it, return
       all nodes contained in the collection, in order. *)
(*!!!    val nodes_of_collection : store * node -> node list *)
end

structure StoreCollection : STORE_COLLECTION = struct

    structure S = Store
    type store = S.t
    datatype node = datatype RdfNode.node
    type triple = node * node * node

    val node_first = RdfNode.IRI RdfStandardIRIs.iri_rdf_first
    val node_rest  = RdfNode.IRI RdfStandardIRIs.iri_rdf_rest
    val node_nil   = RdfNode.IRI RdfStandardIRIs.iri_rdf_nil
                                    
    fun is_collection_node (store, node) =
        let val pat = (S.KNOWN node, S.KNOWN node_rest, S.WILDCARD)
        in
            not (null (S.match (store, pat)))
        end

    fun start_of_collection (store, node) =
        let val pat = (S.WILDCARD, S.KNOWN node_rest, S.KNOWN node)
        in
            case S.match (store, pat) of
                (subj, _, _)::_ => start_of_collection (store, subj)
              | _ => node
        end

    fun triples_of_collection (store, node) =
        let fun triples' (store, first, acc) =
                if first = node_nil
                then acc
                else
                    let val here = (S.KNOWN first, S.KNOWN node_first, S.WILDCARD)
                        val rest = (S.KNOWN first, S.KNOWN node_rest, S.WILDCARD)
                    in
                        case (S.match (store, here), S.match (store, rest)) of
                            ([], []) => acc
                          | (h::_, []) => acc @ [h]
                          | ([], r::_) => triples' (store, #3 r, acc @ [r])
                          | (h::_, r::_) => triples' (store, #3 r, acc @ [h, r])
                    end
        in
            triples' (store, (start_of_collection (store, node)), [])
        end
(*!!!
    fun nodes_of_collection (store, node) =
        let fun collect' (store, first) =
*)                
end
                                
