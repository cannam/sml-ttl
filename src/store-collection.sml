
structure StoreCollection = struct

    fun is_collection_node (store, node) =
        let val pat = (Store.KNOWN node,
                       Store.KNOWN (RdfNode.IRI RdfStandardIRIs.iri_rdf_rest),
                       Store.WILDCARD)
        in
            not (null (Store.match (store, pat)))
        end
            
end
                                
