
signature TEST_COLLECTION_ARG = sig

    structure R : RDF_COLLECTION
    structure C : STORE_COLLECTION
    
end

functor TestCollectionFn (Arg : TEST_COLLECTION_ARG) :> TESTS = struct

    open TestSupport

    structure R = Arg.R
    structure C = Arg.C

    open RdfNode
    open RdfStandardIRIs
                      
    val name = "collection"

    fun scrub_blanks tt =
        let fun scrub (BLANK b) = BLANK 1
              | scrub node = node
        in
            map (fn (subj, pred, obj) => (scrub subj, pred, scrub obj)) tt
        end
                              
    fun check_triples (a, b) =
        check RdfTriple.string_of_triples (scrub_blanks a, scrub_blanks b)

    fun test_make_empty () =
        check_triples
            (R.collection_of_nodes [], [])

    fun test_make_single () =
        check_triples
            (R.collection_of_nodes [IRI (Iri.fromString "x")],
             [(BLANK 1, IRI iri_rdf_first, IRI (Iri.fromString "x")),
              (BLANK 1, IRI iri_rdf_rest, IRI iri_rdf_nil)])

    (*!!! + the other three *)
            
    fun tests () = [
        ("make-empty", test_make_empty),
        ("make-single", test_make_single)
    ]

end
                                                              
structure TestCollection = TestCollectionFn(struct
                                             structure R = RdfCollection
                                             structure C = StoreCollection
                                             end)

