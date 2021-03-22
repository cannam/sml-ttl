
structure TestCollection :> TESTS = struct

    open TestSupport

    structure E = CollectionExpander
    structure G = CollectionGathererFn(Store)

    open RdfNode
    open RdfStandardIRIs
                      
    type test = string * (unit -> bool)
                             
    val name = "collection"

    (* Renumber blank nodes, starting from 1 each time this is called *)
    fun reduceBlanks tt =
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
                              
    fun checkTriples (a, b) =
        check RdfTriple.stringOfTriples (reduceBlanks a, reduceBlanks b)

    fun testMakeEmpty () =
        checkTriples
            (E.collectionOfNodes [], [])

    fun testMakeSingle () =
        checkTriples
            (E.collectionOfNodes [IRI (Iri.fromString "x")],
             [(BLANK 1, IRI iriRdfFirst, IRI (Iri.fromString "x")),
              (BLANK 1, IRI iriRdfRest, IRI iriRdfNil)])

    fun testMakeCollection () =
        checkTriples
            (E.collectionOfNodes [IRI (Iri.fromString "x"),
                                    IRI (Iri.fromString "y")],
             [(BLANK 1, IRI iriRdfFirst, IRI (Iri.fromString "x")),
              (BLANK 1, IRI iriRdfRest, BLANK 2),
              (BLANK 2, IRI iriRdfFirst, IRI (Iri.fromString "y")),
              (BLANK 2, IRI iriRdfRest, IRI iriRdfNil)])

    fun testStartOfCollection () =
        let val tt = E.collectionOfNodes [IRI (Iri.fromString "x"),
                                            IRI (Iri.fromString "y"),
                                            IRI (Iri.fromString "z")]
            val store = foldl (fn (t, s) => Store.add (s, t)) Store.empty tt
            val firstLinkNode = #1 (hd tt) (* subj node of "x" triple *)
            val lastLinkNode = #1 (hd (rev tt)) (* subj node of "z" triple *)
        in
            check RdfNode.stringOfNode
                  (G.startOfCollection (store, lastLinkNode),
                   firstLinkNode)
            andalso
            check RdfNode.stringOfNode
                  (G.startOfCollection (store, firstLinkNode),
                   firstLinkNode)
        end
            
    fun testTriplesOfCollection () =
        let val tt = E.collectionOfNodes [IRI (Iri.fromString "x"),
                                            IRI (Iri.fromString "y")]
            val store = foldl (fn (t, s) => Store.add (s, t)) Store.empty tt
            val lastLinkNode = #1 (hd (rev tt)) (* subj node of "y" triple *)
        in
            checkTriples
                (G.triplesOfCollection (store, lastLinkNode),
                 [(BLANK 1, IRI iriRdfFirst, IRI (Iri.fromString "x")),
                  (BLANK 1, IRI iriRdfRest, BLANK 2),
                  (BLANK 2, IRI iriRdfFirst, IRI (Iri.fromString "y")),
                  (BLANK 2, IRI iriRdfRest, IRI iriRdfNil)])
        end
            
    fun tests () = [
        ("make-empty", testMakeEmpty),
        ("make-single", testMakeSingle),
        ("make-collection", testMakeCollection),
        ("start-of-collection", testStartOfCollection),
        ("triples-of-collection", testTriplesOfCollection)
    ]

end

