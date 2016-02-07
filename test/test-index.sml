
signature TEST_INDEX_ARG = sig

    structure IX : INDEX
    
end

functor TestIndexFn (Arg : TEST_INDEX_ARG) :> TESTS = struct

    open TestSupport

    structure IX = Arg.IX
    structure P = IndexPickerFn(IX)

    val name = "index"

    fun iri i = RdfNode.IRI (Iri.fromString i)
    fun known i = SOME (RdfNode.IRI (Iri.fromString i))
    val wildcard = NONE

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

    val index_names = [ "spo", "pos", "ops", "sop", "pso", "osp" ]

    fun match (index, pattern) =
        IX.foldl_match (fn (t, acc) => t :: acc)
                       []
                       (index, pattern)

    fun enumerate index =
        match (index, make_test_pattern "***")

    fun new_index_for name =
        IX.new (IX.order_of_name name)

    fun build_index name templates =
        List.foldl (fn (t, ix) => IX.add (ix, make_test_triple t))
                   (new_index_for name)
                   templates

    fun check_triple_lists (a, b) =
        check_sets RdfTriple.string_of_triple
                   (fn (t1, t2) => RdfTriple.compare (t1, t2) = GREATER)
                   (a, b)

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
                check_pairs Bool.toString
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
                check_pairs Bool.toString
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
                    (match (ix, make_test_pattern "***"),
                     enumerate ix)
                andalso
                check_triple_lists
                    (match (ix, make_test_pattern "**a"),
                     [make_test_triple "aaa", make_test_triple "bba"])
            end)
    ]
                          
    val named_tests = List.concat (map index_tests index_names)

    fun check_picked_index ixs pattern names =
        (* Check that the index picked from ixs for a pattern is one
           of the expected set given by names *)
        let val ix = P.pick_index (ixs, make_test_pattern pattern)
        in
            case List.find (fn n => (IX.name ix = n)) names of
                SOME _ => true
              | NONE => (report (fn x => x)
                                ("one of: " ^ (String.concatWith ", " names),
                                 IX.name ix);
                         false)
        end
                                  
    fun test_choose_index_from_all () =
        let val ixs = List.map new_index_for index_names
        in
            check_picked_index ixs "aa*" ["spo", "pso"] andalso
            check_picked_index ixs "a**" ["spo", "sop"] andalso
            check_picked_index ixs "aaa" index_names andalso
            check_picked_index ixs "a*a" ["sop", "osp"] andalso
            check_picked_index ixs "***" index_names andalso
            check_picked_index ixs "*a*" ["pso", "pos"]
        end

    fun test_choose_index_from_subset () =
        let val ixs = List.map new_index_for [ "spo", "pos", "ops" ]
        in
            check_picked_index ixs "aa*" ["spo"] andalso
            check_picked_index ixs "a**" ["spo"] andalso
            check_picked_index ixs "aaa" ["spo", "pos", "ops"] andalso
            check_picked_index ixs "a*a" ["ops", "spo"] andalso
            check_picked_index ixs "***" ["spo", "pos", "ops"] andalso
            check_picked_index ixs "*a*" ["pos"]
        end
                                  
    fun tests () =
        (named_tests @
         [
           ("choose-index-from-all", test_choose_index_from_all),
           ("choose-index-from-subset", test_choose_index_from_subset)
        ])
             
end

structure TestIndex = TestIndexFn(struct
                                   structure IX = Index
                                   end)
                                 
