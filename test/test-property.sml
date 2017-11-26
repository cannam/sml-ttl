
functor TestPropertyFn (P: STORE_PROPERTY) :> TESTS = struct

    open TestSupport
    open RdfNode
             
    val name = "property";

    val testfile = "test/other/goblin.ttl"
    val goblinIri = Iri.fromString "http://example.org/#green-goblin"
    val spiderIri = Iri.fromString "http://example.org/#spiderman"
    val goblinNode = IRI goblinIri
    val spiderNode = IRI spiderIri
    val spiderRu = WdString.implodeToUtf8 [
            0wx427, 0wx435, 0wx43B, 0wx43E, 0wx432, 0wx435, 0wx43A, 0wx2D,
            0wx43F, 0wx430, 0wx443, 0wx43A ]
                           
    fun loadTestfile () =
        let open StoreFileLoader
        in
            case loadFileAsNewStore (NONE, testfile) of
                FORMAT_NOT_SUPPORTED =>
                (print "--- Test file format not supported!\n"; NONE)
              | SYSTEM_ERROR err => 
                (print ("--- Failed to open test file \"" ^ testfile ^
                        "\": " ^ err ^ "\n"); NONE)
              | PARSE_ERROR err =>
                (print ("--- Failed to parse test file \"" ^ testfile ^
                        "\": " ^ err ^ "\n"); NONE)
              | OK store => SOME store
        end

    fun withCodepoints s =
        s ^ " [ " ^
        (String.concatWith " " (map Word.toString (WdString.explodeUtf8 s)))
        ^ " ]";
                
    structure Sort = ListMergeSort

    fun tests () = [
        ("text",
         fn () =>
            case loadTestfile () of
                NONE => false
              | SOME store =>
                check (fn x => x)
                      (P.text (store, goblinNode, "foaf:name"),
                       "Green Goblin")),
        ("text list",
         fn () =>
            case loadTestfile () of
                NONE => false
              | SOME store =>
                check (fn x => withCodepoints (String.concatWith "," x))
                      (Sort.sort String.>
                       (P.textList (store, spiderNode, "foaf:name")),
                       ["Spiderman", spiderRu])),
        ("iri",
         fn () =>
            case loadTestfile () of
                NONE => false
              | SOME store =>
                check Iri.toString
                      (case P.iri (store, goblinNode, "rel:enemyOf") of
                           SOME iri => iri
                         | NONE => Iri.empty,
                       spiderIri)),
        ("iriList",
         fn () =>
            case loadTestfile () of
                NONE => false
              | SOME store =>
                check (fn x => String.concatWith "," (map Iri.toString x))
                      (P.iriList (store, spiderNode, "rel:enemyOf"),
                       [goblinIri])),
        ("node",
         fn () =>
            case loadTestfile () of
                NONE => false
              | SOME store =>
                check (fn x => x)
                      (case P.node (store, goblinNode, "rel:enemyOf") of
                           SOME node => RdfNode.stringOfNode node
                         | NONE => "",
                       RdfNode.stringOfNode spiderNode)),
        ("nodeList",
         fn () =>
            case loadTestfile () of
                NONE => false
              | SOME store =>
                check (fn x => String.concatWith
                                   "," (map RdfNode.stringOfNode x))
                      (P.nodeList (store, spiderNode, "rel:enemyOf"),
                       [goblinNode]))
    ]

end

structure TestProperty = TestPropertyFn(StoreProperty)
