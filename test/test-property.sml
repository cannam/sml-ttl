
functor TestPropertyFn (P: STORE_PROPERTY) :> TESTS = struct

    open TestSupport
    open RdfNode
             
    val name = "property";

    val testfile = "test/other/goblin.ttl"
    val goblin_iri = Iri.fromString "http://example.org/#green-goblin"
    val spider_iri = Iri.fromString "http://example.org/#spiderman"
    val goblin_node = IRI goblin_iri
    val spider_node = IRI spider_iri
    val spider_ru = WdString.implodeToUtf8 [
            0wx427, 0wx435, 0wx43B, 0wx43E, 0wx432, 0wx435, 0wx43A, 0wx2D,
            0wx43F, 0wx430, 0wx443, 0wx43A ]
                           
    fun load_testfile () =
        let open StoreFileLoader
        in
            case load_file_as_new_store "" testfile of
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

    fun with_codepoints s =
        s ^ " [ " ^
        (String.concatWith " " (map Word.toString (WdString.explodeUtf8 s)))
        ^ " ]";
                
    structure Sort = ListMergeSort

    fun tests () = [
        ("text",
         fn () =>
            case load_testfile () of
                NONE => false
              | SOME store =>
                check (fn x => x)
                      (P.text (store, goblin_node, "foaf:name"),
                       "Green Goblin")),
        ("text list",
         fn () =>
            case load_testfile () of
                NONE => false
              | SOME store =>
                check (fn x => with_codepoints (String.concatWith "," x))
                      (Sort.sort String.>
                       (P.text_list (store, spider_node, "foaf:name")),
                       ["Spiderman", spider_ru])),
        ("iri",
         fn () =>
            case load_testfile () of
                NONE => false
              | SOME store =>
                check Iri.toString
                      (case P.iri (store, goblin_node, "rel:enemyOf") of
                           SOME iri => iri
                         | NONE => Iri.empty,
                       spider_iri)),
        ("iri_list",
         fn () =>
            case load_testfile () of
                NONE => false
              | SOME store =>
                check (fn x => String.concatWith "," (map Iri.toString x))
                      (P.iri_list (store, spider_node, "rel:enemyOf"),
                       [goblin_iri])),
        ("node",
         fn () =>
            case load_testfile () of
                NONE => false
              | SOME store =>
                check (fn x => x)
                      (case P.node (store, goblin_node, "rel:enemyOf") of
                           SOME node => RdfNode.string_of_node node
                         | NONE => "",
                       RdfNode.string_of_node spider_node)),
        ("node_list",
         fn () =>
            case load_testfile () of
                NONE => false
              | SOME store =>
                check (fn x => String.concatWith
                                   "," (map RdfNode.string_of_node x))
                      (P.node_list (store, spider_node, "rel:enemyOf"),
                       [goblin_node]))
    ]

end

structure TestProperty = TestPropertyFn(StoreProperty)
