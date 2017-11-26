
structure CollectionExpander :> COLLECTION_EXPANDER = struct

    open RdfNode
    open RdfTriple
    open RdfStandardIRIs

    (* Given a list of nodes, return a list of triples comprising the
       RDF collection of those nodes. *)
    fun collectionOfNodes nodes =
	let fun collection' link [] = []
	      | collection' link [node] =
		( link, IRI iriRdfFirst, node ) ::
		( link, IRI iriRdfRest, IRI iriRdfNil ) :: []
	      | collection' link (node::nodes) =
		let val blank = newBlankNode () in
		    ( link, IRI iriRdfFirst, node ) ::
		    ( link, IRI iriRdfRest, blank ) :: (collection' blank nodes)
		end
	in
	    collection' (newBlankNode ()) nodes
	end
	     
end

                           
