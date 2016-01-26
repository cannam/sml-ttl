
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

    (* Renumber blank nodes, starting from 1 each time this is called *)
    fun reduce_blanks tt =
        let open IntRedBlackMap
            val map = ref empty
            val n = ref 0
            fun reduce (BLANK b) =
                (case find (!map, b) of
                     SOME replacement => BLANK replacement
                   | NONE =>
                     (n := 1 + !n;
                      map := insert (!map, b, !n);
                      BLANK (!n)))
              | reduce node = node
        in
            List.map (fn (subj, pred, obj) => (reduce subj, pred, reduce obj)) tt
        end
                              
    fun check_triples (a, b) =
        check RdfTriple.string_of_triples (reduce_blanks a, reduce_blanks b)

    fun test_make_empty () =
        check_triples
            (R.collection_of_nodes [], [])

    fun test_make_single () =
        check_triples
            (R.collection_of_nodes [IRI (Iri.fromString "x")],
             [(BLANK 1, IRI iri_rdf_first, IRI (Iri.fromString "x")),
              (BLANK 1, IRI iri_rdf_rest, IRI iri_rdf_nil)])

    fun test_make_collection () =
        check_triples
            (R.collection_of_nodes [IRI (Iri.fromString "x"),
                                    IRI (Iri.fromString "y")],
             [(BLANK 1, IRI iri_rdf_first, IRI (Iri.fromString "x")),
              (BLANK 1, IRI iri_rdf_rest, BLANK 2),
              (BLANK 2, IRI iri_rdf_first, IRI (Iri.fromString "y")),
              (BLANK 2, IRI iri_rdf_rest, IRI iri_rdf_nil)])
            
    (*!!! + the other two *)
            
    fun tests () = [
        ("make-empty", test_make_empty),
        ("make-single", test_make_single),
        ("make-collection", test_make_collection)
    ]

end
                                                              
structure TestCollection = TestCollectionFn(struct
                                             structure R = RdfCollection
                                             structure C = StoreCollection
                                             end)

