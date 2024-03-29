
functor CollectionGathererFn (M : MATCHER) :> COLLECTION_GATHERER
                                                  where type matcher = M.t
= struct

    type matcher = M.t
    datatype node = datatype RdfNode.node
    type triple = node * node * node

    open RdfLog
             
    val nodeFirst = RdfNode.IRI RdfStandardIRIs.iriRdfFirst
    val nodeRest  = RdfNode.IRI RdfStandardIRIs.iriRdfRest
    val nodeNil   = RdfNode.IRI RdfStandardIRIs.iriRdfNil
                                    
    fun isCollectionNode (matcher, node) =
        node = nodeNil orelse
        let val pat = (SOME node, SOME nodeRest, NONE)
            val result = not (null (M.match (matcher, pat)))
        in
            debug (fn () => ["Collection: %1 %2 a collection node",
                             S (RdfNode.stringOfNode node),
                             S (if result then "is" else "is not")]);
            result
        end

    fun startOfCollection (matcher, node) =
        let val pat = (NONE, SOME nodeRest, SOME node)
        in
            case M.match (matcher, pat) of
                (subj, _, _)::_ => startOfCollection (matcher, subj)
              | _ => node
        end

    fun triplesOfCollection (matcher, node) =
        let fun triples' (matcher, first, acc) =
                if first = nodeNil
                then acc
                else
                    let val here = (SOME first, SOME nodeFirst, NONE)
                        val rest = (SOME first, SOME nodeRest, NONE)
                    in
                        case (M.match (matcher, here), M.match (matcher, rest)) of
                            ([], []) => acc
                          | (h::_, []) => acc @ [h]
                          | ([], r::_) => triples' (matcher, #3 r, acc @ [r])
                          | (h::_, r::_) => triples' (matcher, #3 r, acc @ [h, r])
                    end
            val result = triples' (matcher, (startOfCollection (matcher, node)), [])
        in
            debug (fn () => ["Collection: node %1 yields collection:\n%",
                             S (RdfNode.stringOfNode node),
                             S (RdfTriple.stringOfTriples result)]);
            result
        end

    fun nodesOfCollection (matcher, node) =
        List.concat
            (map (fn t => if (#2 t = nodeFirst) then [#3 t] else [])
                 (triplesOfCollection (matcher, node)))

end
     
