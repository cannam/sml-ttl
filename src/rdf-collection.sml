
structure RdfCollection = struct

    open RdfTriple
    open RdfStandardIRIs

    fun collection_of_nodes nodes =
	let fun collection' link [] = []
	      | collection' link [node] =
		( link, IRI iri_rdf_first, node ) ::
		( link, IRI iri_rdf_rest, IRI iri_rdf_nil ) :: []
	      | collection' link (node::nodes) =
		let val blank = new_blank_node () in
		    ( link, IRI iri_rdf_first, node ) ::
		    ( link, IRI iri_rdf_rest, blank ) :: (collection' blank nodes)
		end
	in
	    collection' (new_blank_node ()) nodes
	end
	     
end

			   
