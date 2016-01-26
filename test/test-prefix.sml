
functor TestPrefixFn (P: PREFIX_TABLE) :> TESTS = struct

    open TestSupport

    val name = "prefix"
                                  
    fun make_table pairs =
        foldl (fn ((p, e), t) => P.add (t, p, e)) P.empty pairs
             
    val unreal_table =
        make_table
            [("1", "a"),
             ("2", "ab"),
             ("3", "aba"),
             ("4", "abba"),
             ("5", "abc"),
             ("6", "bbc"),
             ("7", "bbf")]
             
    val real_table =
        make_table 
            [("fruit", "http://example.com/fruit/"),
             ("fruitloop", "http://example.com/fruit/loop/"),
             ("veg", "http://example.com/vegetable/"),
             ("", "empty")]

    val nil_table =
        make_table
            [("", "")]
            
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
         fn () => P.enumerate (P.add (P.empty, "a", "b")) = [("a","b")]),
        ("add-another",
         fn () => P.enumerate (P.add (P.add (P.empty, "a", "b"), "aa", "bb")) =
                  [("a","b"),("aa","bb")]), (*!!! compare unordered! *)
        ("replace",
         fn () => P.enumerate (P.add (P.add (P.empty, "a", "b"), "a", "bb")) =
                  [("a","bb")]),
        ("expand",
         fn () =>
            check_all
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
            check_all
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
            check_all
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
