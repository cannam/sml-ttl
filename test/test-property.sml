
functor TestPropertyFn (P: PROPERTY) : TESTS = struct

    open TestSupport
             
    val testfile = "test/other/goblin.ttl"
    val goblin = Store.IRI (Iri.fromString "http://example.org/#green-goblin")
    val spider = Store.IRI (Iri.fromString "http://example.org/#spiderman")
    val spider_ru = WdString.implodeToUtf8 [
            0wx427, 0wx435, 0wx43B, 0wx43E, 0wx432, 0wx435, 0wx43A, 0wx2D,
            0wx43F, 0wx430, 0wx443, 0wx43A ]
                           
    fun load_testfile () =
        case StoreFileLoader.load_file_as_new_store "" testfile of
            StoreFileLoader.OK store => SOME store
          | StoreFileLoader.LOAD_ERROR err =>
            (print ("--- Failed to load test file \"" ^ testfile ^
                    "\": " ^ err ^ "\n");
             NONE)

    fun with_codepoints s =
        s ^ " [ " ^
        (String.concatWith " " (map Word.toString (WdString.explodeUtf8 s)))
        ^ " ]";
                
    structure Sort = ListMergeSort

    (*!!! separate out name accessor from test list? *)
    (*!!! make tests a thunk, so it doesn't get evaluated at compile time? *)
    val tests = (
        "property",
        [
          ("text",
           fn () =>
              case load_testfile () of
                  NONE => false
                | SOME store =>
                  check (fn x => x)
                        (P.text (store, goblin, "foaf:name"),
                         "Green Goblin")),
          ("text_list",
           fn () =>
              case load_testfile () of
                  NONE => false
                | SOME store =>
                  check (fn x => with_codepoints (String.concatWith "," x))
                        (Sort.sort String.>
                         (P.text_list (store, spider, "foaf:name")),
                         ["Spiderman", spider_ru]))
        ]
    )

end

structure TestProperty = TestPropertyFn(Property)
