
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

    fun test_start_of_collection () =
        let val tt = R.collection_of_nodes [IRI (Iri.fromString "x"),
                                            IRI (Iri.fromString "y"),
                                            IRI (Iri.fromString "z")]
            val store = foldl (fn (t, s) => Store.add (s, t)) Store.empty tt
            val first_link_node = #1 (hd tt) (* subj node of "x" triple *)
            val last_link_node = #1 (hd (rev tt)) (* subj node of "z" triple *)
        in
            check RdfTriple.string_of_node
                  (C.start_of_collection (store, last_link_node),
                   first_link_node)
            andalso
            check RdfTriple.string_of_node
                  (C.start_of_collection (store, first_link_node),
                   first_link_node)
        end
            
    fun test_triples_of_collection () =
        let val tt = R.collection_of_nodes [IRI (Iri.fromString "x"),
                                            IRI (Iri.fromString "y")]
            val store = foldl (fn (t, s) => Store.add (s, t)) Store.empty tt
            val last_link_node = #1 (hd (rev tt)) (* subj node of "y" triple *)
        in
            check_triples
                (C.triples_of_collection (store, last_link_node),
                 [(BLANK 1, IRI iri_rdf_first, IRI (Iri.fromString "x")),
                  (BLANK 1, IRI iri_rdf_rest, BLANK 2),
                  (BLANK 2, IRI iri_rdf_first, IRI (Iri.fromString "y")),
                  (BLANK 2, IRI iri_rdf_rest, IRI iri_rdf_nil)])
        end
            
    fun tests () = [
        ("make-empty", test_make_empty),
        ("make-single", test_make_single),
        ("make-collection", test_make_collection),
        ("start-of-collection", test_start_of_collection),
        ("triples-of-collection", test_triples_of_collection)
    ]

end
                                                              
structure TestCollection = TestCollectionFn(struct
                                             structure R = RdfCollection
                                             structure C = StoreCollection
                                             end)

