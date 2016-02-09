
functor TestPrefixFn (P: PREFIX_TABLE) :> TESTS = struct

    open TestSupport

    val name = "prefix"
                                  
    fun make_table pairs =
        foldl (fn ((p, e), t) => P.add (t, p, e)) P.empty pairs
             
    val unreal_table =
        make_table
            [("1", Iri.fromString "a"),
             ("2", Iri.fromString "ab"),
             ("3", Iri.fromString "aba"),
             ("4", Iri.fromString "abba"),
             ("5", Iri.fromString "abc"),
             ("6", Iri.fromString "bbc"),
             ("7", Iri.fromString "bbf")]
             
    val real_table =
        make_table 
            [("fruit", Iri.fromString "http://example.com/fruit/"),
             ("fruitloop", Iri.fromString "http://example.com/fruit/loop/"),
             ("veg", Iri.fromString "http://example.com/vegetable/"),
             ("", Iri.fromString "empty")]

    val nil_table =
        make_table
            [("", Iri.fromString "")]
            
    fun abbr_unreal s = P.abbreviate (unreal_table, Iri.fromString s)
    fun abbr_real s = P.abbreviate (real_table, Iri.fromString s)

    fun abbr_to_string (SOME (ns, loc)) = ns ^ ":" ^ loc
      | abbr_to_string NONE = "(none)"

    fun tests () = [
        ("empty",
         fn () => null (P.enumerate P.empty)),
        ("empty-expand",
         fn () => P.expand (P.empty, "a:b") = Iri.fromString "a:b"),
        ("empty-abbreviate",
         fn () => P.abbreviate (P.empty, Iri.fromString "a:b") = NONE),
        ("add",
         fn () => P.enumerate (P.add (P.empty, "a", Iri.fromString "b")) = [("a", Iri.fromString "b")]),
        ("add-another",
         fn () => P.enumerate (P.add (P.add (P.empty, "a", Iri.fromString "b"), "aa", Iri.fromString "bb")) =
                  [("a", Iri.fromString "b"),("aa", Iri.fromString "bb")]), (*!!! compare unordered! *)
        ("replace",
         fn () => P.enumerate (P.add (P.add (P.empty, "a", Iri.fromString "b"), "a", Iri.fromString "bb")) =
                  [("a", Iri.fromString "bb")]),
        ("expand",
         fn () =>
            check_pairs
                Iri.toString
                [(P.expand (real_table, "veg:aubergine"),
                  Iri.fromString "http://example.com/vegetable/aubergine"),
                 (P.expand (real_table, "fruit:banana:thing"),
                  Iri.fromString "http://example.com/fruit/banana:thing"),
                 (P.expand (real_table, "fruitloop:banana"),
                  Iri.fromString "http://example.com/fruit/loop/banana"),
                 (P.expand (real_table, ":banana"),
                  Iri.fromString "emptybanana"),
                 (P.expand (nil_table, "a.b"),
                  Iri.fromString "a.b"),
                 (P.expand (nil_table, ":a.b"),
                  Iri.fromString "a.b")]
        ),
        ("abbreviate",
         fn () =>
            check_pairs
                abbr_to_string
                [(abbr_unreal "a", SOME ("1", "")),
                 (abbr_unreal "aa", SOME ("1", "a")),
                 (abbr_unreal "abc", SOME ("5", "")),
                 (abbr_unreal "abd", SOME ("2", "d")),
                 (abbr_unreal "bbb", NONE),
                 (abbr_unreal "bbd", NONE),
                 (abbr_unreal "aba", SOME ("3", "")),
                 (abbr_unreal "abad", SOME ("3", "d"))]
        ),
        ("abbreviate-lifelike",
         fn () =>
            check_pairs
                abbr_to_string
                [(abbr_real "http://example.com/vegetable/aubergine", SOME ("veg", "aubergine")),
                 (abbr_real "http://example.com/fruit/banana:thing", SOME ("fruit", "banana:thing")),
                 (abbr_real "http://example.com/fruit/loop/banana", SOME ("fruitloop", "banana")),
                 (abbr_real "http://example.com/fruit/loop:banana", SOME ("fruit", "loop:banana")),
                 (abbr_real "http://example.com/fruit/", SOME ("fruit", "")),
                 (abbr_real "fruit", NONE),
                 (abbr_real "emptyfruit", SOME ("", "fruit"))]
        )
    ]

end

structure TestPrefix = TestPrefixFn(PrefixTable)
