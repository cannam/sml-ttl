
functor CollectionGathererFn (M : MATCHER) :> COLLECTION_GATHERER
                                                  where type matcher = M.t
= struct

    type matcher = M.t
    datatype node = datatype RdfNode.node
    type triple = node * node * node

    val node_first = RdfNode.IRI RdfStandardIRIs.iri_rdf_first
    val node_rest  = RdfNode.IRI RdfStandardIRIs.iri_rdf_rest
    val node_nil   = RdfNode.IRI RdfStandardIRIs.iri_rdf_nil
                                    
    fun is_collection_node (matcher, node) =
        let val pat = (SOME node, SOME node_rest, NONE)
            val result = not (null (M.match (matcher, pat)))
        in
            Log.info (fn () => ["Collection: % % a collection node",
                                Log.S (RdfNode.string_of_node node),
                                Log.S (if result then "is" else "is not")]);
            result
        end

    fun start_of_collection (matcher, node) =
        let val pat = (NONE, SOME node_rest, SOME node)
        in
            case M.match (matcher, pat) of
                (subj, _, _)::_ => start_of_collection (matcher, subj)
              | _ => node
        end

    fun triples_of_collection (matcher, node) =
        let fun triples' (matcher, first, acc) =
                if first = node_nil
                then acc
                else
                    let val here = (SOME first, SOME node_first, NONE)
                        val rest = (SOME first, SOME node_rest, NONE)
                    in
                        case (M.match (matcher, here), M.match (matcher, rest)) of
                            ([], []) => acc
                          | (h::_, []) => acc @ [h]
                          | ([], r::_) => triples' (matcher, #3 r, acc @ [r])
                          | (h::_, r::_) => triples' (matcher, #3 r, acc @ [h, r])
                    end
            val result = triples' (matcher, (start_of_collection (matcher, node)), [])
        in
            Log.info (fn () => ["Collection: node % yields collection:\n%",
                                Log.S (RdfNode.string_of_node node),
                                Log.S (RdfTriple.string_of_triples result)]);
            result
        end

    fun nodes_of_collection (matcher, node) =
        List.concat
            (map (fn t => if (#2 t = node_first) then [#3 t] else [])
                 (triples_of_collection (matcher, node)))

end
     
