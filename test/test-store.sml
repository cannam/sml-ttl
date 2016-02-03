
signature TEST_STORE_ARG = sig

    structure S : STORE
    
end

functor TestStoreFn (Arg : TEST_STORE_ARG) :> TESTS = struct

    (* Basically a very cut-down version of TestIndexFn. Some
       duplication here could be avoided if Store and Index had a
       common signature, but that would spoil the API since Index is
       not really intended for public use *)
    
    open TestSupport

    structure S = Arg.S

    val name = "store"

    fun iri i = RdfNode.IRI (Iri.fromString i)
    fun known i = S.KNOWN (RdfNode.IRI (Iri.fromString i))
    val wildcard = S.WILDCARD

    fun make_test_triple "aaa" = (iri "fred", iri "loves", iri "cheese")
      | make_test_triple "aab" = (iri "fred", iri "loves", iri "fruit")
      | make_test_triple "abc" = (iri "fred", iri "hates", iri "vinegar")
      | make_test_triple "bba" = (iri "jodie", iri "hates", iri "cheese")
      | make_test_triple "bca" = (iri "jodie", iri "tolerates", iri "fred")
      | make_test_triple x = raise Fail ("Unsupported triple template " ^ x)
                       
    fun make_test_pattern "***" = (wildcard, wildcard, wildcard)
      | make_test_pattern "aaa" = (known "fred", known "loves", known "cheese")
      | make_test_pattern "aa*" = (known "fred", known "loves", wildcard)
      | make_test_pattern "a**" = (known "fred", wildcard, wildcard)
      | make_test_pattern "*a*" = (wildcard, known "loves", wildcard)
      | make_test_pattern "a*a" = (known "fred", wildcard, known "cheese")
      | make_test_pattern "**a" = (wildcard, wildcard, known "cheese")
      | make_test_pattern x = raise Fail ("Unsupported wildcard template " ^ x)

    fun match (store, pattern) =
        S.fold_match (fn (t, acc) => t :: acc)
                      []
                      (store, pattern)

    fun build_store name templates =
        List.foldl (fn (t, st) => S.add (st, make_test_triple t))
                   S.empty
                   templates
                    
    fun check_lists conv ([], []) = true
      | check_lists conv (a::aa, []) =
        (print ("--- Lists have differing lengths (reached " ^ (conv a) ^
                ", had expected end of list)");
         false)
      | check_lists conv ([], b::bb) =
        (print ("--- Lists have differing lengths (reached end of list, " ^
                "had expected " ^ (conv b) ^ ")");
         false)
      | check_lists conv (a::aa, b::bb) =
        check conv (a, b) andalso check_lists conv (aa, bb)

    structure Sort = ListMergeSort

    fun triple_greater (t1, t2) = RdfTriple.compare (t1, t2) = GREATER

    fun check_triple_lists (a, b) =
        check_lists RdfTriple.string_of_triple
                    (Sort.sort triple_greater a,
                     Sort.sort triple_greater b)

    fun tests () = [

        ("store-empty",
         fn () => check_triple_lists
                      (S.enumerate S.empty,
                       [])),
        
        ("store-simple",
         fn () => check_triple_lists
                      (S.enumerate (build_store name ["aaa"]),
                       [make_test_triple "aaa"])),

        ("store-duplicate",
         fn () => check_triple_lists
                      (S.enumerate (build_store name ["aaa", "aaa"]),
                       [make_test_triple "aaa"])),

        ("store-common",
         fn () => check_triple_lists
                      (S.enumerate (build_store name ["aaa", "aab"]),
                       [make_test_triple "aab", make_test_triple "aaa"])),

        ("store-diverging",
         fn () => check_triple_lists
                      (S.enumerate (build_store name ["aaa", "aab", "abc", "bba"]),
                       [make_test_triple "abc",
                        make_test_triple "aaa",
                        make_test_triple "aab",
                        make_test_triple "bba"])),
        
        ("store-contains",
         fn () =>
            let val st = build_store name ["aaa", "aab", "abc", "bba"]
            in
                check_pairs Bool.toString
                          [(S.contains (st, make_test_triple "aaa"), true),
                           (S.contains (st, make_test_triple "aab"), true),
                           (S.contains (st, make_test_triple "abc"), true),
                           (S.contains (st, make_test_triple "bba"), true),
                           (S.contains (st, make_test_triple "bca"), false)]
            end),
        
        ("store-remove",
         fn () =>
            let val st = build_store name ["aaa", "aab", "abc", "bba"]
                val st_r = S.remove (st, make_test_triple "aaa")
                val st_rr = S.remove (S.remove (st, make_test_triple "aaa"),
                                       make_test_triple "aaa")
            in
                check_pairs Bool.toString
                          [(S.contains (st, make_test_triple "aaa"), true),
                           (S.contains (st_r, make_test_triple "aaa"), false),
                           (S.contains (st_rr, make_test_triple "aaa"), false),
                           (S.contains (st_r, make_test_triple "abc"), true),
                           (S.contains (st_rr, make_test_triple "bba"), true)]
            end),

        ("store-match",
         fn () =>
            let val st = build_store name ["aaa", "aab", "abc", "bba"]
            in
                check_triple_lists
                    (match (st, make_test_pattern "***"),
                     S.enumerate st)
                andalso
                check_triple_lists
                    (match (st, make_test_pattern "**a"),
                     [make_test_triple "aaa", make_test_triple "bba"])
            end)
    ]
             
end

structure TestStore = TestStoreFn(struct
                                   structure S = Store
                                   end)
                                 
