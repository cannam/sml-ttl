
signature TEST_INDEX_ARG = sig

    structure IX : INDEX
    structure P : INDEX_PICKER
    
end

functor TestIndexFn (Arg : TEST_INDEX_ARG) :> TESTS = struct

    open TestSupport

    structure IX = Arg.IX
    structure P = Arg.P

    val name = "index"

    fun iri i = RdfNode.IRI (Iri.fromString i)
    fun known i = IX.KNOWN (RdfNode.IRI (Iri.fromString i))
    val wildcard = IX.WILDCARD

    fun make_test_triple "aaa" = (iri "fred", iri "loves", iri "cheese")
      | make_test_triple "aab" = (iri "fred", iri "loves", iri "fruit")
      | make_test_triple "abc" = (iri "fred", iri "hates", iri "vinegar")
      | make_test_triple "bba" = (iri "jodie", iri "hates", iri "cheese")
      | make_test_triple "bca" = (iri "jodie", iri "tolerates", iri "fred")
      | make_test_triple x = raise Fail ("Unsupported triple template " ^ x)
                       
    fun make_test_wildcard "***" = (wildcard, wildcard, wildcard)
      | make_test_wildcard "aaa" = (known "fred", known "loves", known "cheese")
      | make_test_wildcard "aa*" = (known "fred", known "loves", wildcard)
      | make_test_wildcard "a**" = (known "fred", wildcard, wildcard)
      | make_test_wildcard "*a*" = (wildcard, known "loves", wildcard)
      | make_test_wildcard "a*a" = (known "fred", wildcard, known "cheese")
      | make_test_wildcard "**a" = (wildcard, wildcard, known "cheese")
      | make_test_wildcard x = raise Fail ("Unsupported wildcard template " ^ x)

    val index_names = [ "spo", "pos", "ops", "sop", "pso", "osp" ]

    fun match (index, pattern) =
        IX.fold_match (fn (t, acc) => t :: acc)
                      []
                      (index, pattern)

    fun enumerate index =
        match (index, make_test_wildcard "***")

    fun new_index_for name =
        IX.new (IX.order_of_name name)

    fun build_index name templates =
        List.foldl (fn (t, ix) => IX.add (ix, make_test_triple t))
                   (new_index_for name)
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

    fun index_tests name = [

        (name ^ "-empty",
         fn () => check_triple_lists
                      (enumerate (new_index_for name),
                       [])),
        
        (name ^ "-simple",
         fn () => check_triple_lists
                      (enumerate (build_index name ["aaa"]),
                       [make_test_triple "aaa"])),

        (name ^ "-duplicate",
         fn () => check_triple_lists
                      (enumerate (build_index name ["aaa", "aaa"]),
                       [make_test_triple "aaa"])),

        (name ^ "-common",
         fn () => check_triple_lists
                      (enumerate (build_index name ["aaa", "aab"]),
                       [make_test_triple "aab", make_test_triple "aaa"])),

        (name ^ "-diverging",
         fn () => check_triple_lists
                      (enumerate (build_index name ["aaa", "aab", "abc", "bba"]),
                       [make_test_triple "abc",
                        make_test_triple "aaa",
                        make_test_triple "aab",
                        make_test_triple "bba"])),
        
        (name ^ "-contains",
         fn () =>
            let val ix = build_index name ["aaa", "aab", "abc", "bba"]
            in
                check_all Bool.toString
                          [(IX.contains (ix, make_test_triple "aaa"), true),
                           (IX.contains (ix, make_test_triple "aab"), true),
                           (IX.contains (ix, make_test_triple "abc"), true),
                           (IX.contains (ix, make_test_triple "bba"), true),
                           (IX.contains (ix, make_test_triple "bca"), false)]
            end),
        
        (name ^ "-remove",
         fn () =>
            let val ix = build_index name ["aaa", "aab", "abc", "bba"]
                val ix_r = IX.remove (ix, make_test_triple "aaa")
                val ix_rr = IX.remove (IX.remove (ix, make_test_triple "aaa"),
                                       make_test_triple "aaa")
            in
                check_all Bool.toString
                          [(IX.contains (ix, make_test_triple "aaa"), true),
                           (IX.contains (ix_r, make_test_triple "aaa"), false),
                           (IX.contains (ix_rr, make_test_triple "aaa"), false),
                           (IX.contains (ix_r, make_test_triple "abc"), true),
                           (IX.contains (ix_rr, make_test_triple "bba"), true)]
            end),

        (name ^ "-match",
         fn () =>
            let val ix = build_index name ["aaa", "aab", "abc", "bba"]
            in
                check_triple_lists
                    (match (ix, make_test_wildcard "***"),
                     enumerate ix)
                andalso
                check_triple_lists
                    (match (ix, make_test_wildcard "**a"),
                     [make_test_triple "aaa", make_test_triple "bba"])
            end)
    ]
                          
    val named_tests = List.concat (map index_tests index_names)
                                                           
    fun tests () =
        (named_tests @
         [
           ("remember-to-do-index-picker-tests",
            fn () => false)
        ])
             
end

structure TestIndex = TestIndexFn(struct
                                   structure IX = Index
                                   structure P = IndexPicker
                                   end)
                                 
