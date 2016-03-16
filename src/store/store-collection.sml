
structure StoreCollection :> STORE_COLLECTION = struct

    structure S = Store
    type store = S.t
    datatype node = datatype RdfNode.node
    type triple = node * node * node

    val node_first = RdfNode.IRI RdfStandardIRIs.iri_rdf_first
    val node_rest  = RdfNode.IRI RdfStandardIRIs.iri_rdf_rest
    val node_nil   = RdfNode.IRI RdfStandardIRIs.iri_rdf_nil
                                    
    fun is_collection_node (store, node) =
        let val pat = (SOME node, SOME node_rest, NONE)
            val result = not (null (S.match (store, pat)))
        in
            Log.info (fn () => ("StoreCollection: % % a collection node",
                                [Log.S (RdfNode.string_of_node node),
                                 Log.S (if result then "is" else "is not")]));
            result
        end

    fun start_of_collection (store, node) =
        let val pat = (NONE, SOME node_rest, SOME node)
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
                    let val here = (SOME first, SOME node_first, NONE)
                        val rest = (SOME first, SOME node_rest, NONE)
                    in
                        case (S.match (store, here), S.match (store, rest)) of
                            ([], []) => acc
                          | (h::_, []) => acc @ [h]
                          | ([], r::_) => triples' (store, #3 r, acc @ [r])
                          | (h::_, r::_) => triples' (store, #3 r, acc @ [h, r])
                    end
            val result = triples' (store, (start_of_collection (store, node)), [])
        in
            Log.info (fn () => ("StoreCollection: node % yields collection:\n%",
                                [Log.S (RdfNode.string_of_node node),
                                 Log.S (RdfTriple.string_of_triples result)]));
            result
        end

    fun nodes_of_collection (store, node) =
        List.concat
            (map (fn t => if (#2 t = node_first) then [#3 t] else [])
                 (triples_of_collection (store, node)))

end
                                
