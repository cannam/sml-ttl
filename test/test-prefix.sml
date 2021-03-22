
functor TestPrefixFn (P: PREFIX_TABLE) :> TESTS = struct

    open TestSupport

    type test = string * (unit -> bool)
                             
    val name = "prefix"
                                  
    fun makeTable pairs =
        foldl (fn ((p, e), t) => P.add (t, (p, e))) P.empty pairs
             
    val unrealTable =
        makeTable
            [("1", Iri.fromString "a"),
             ("2", Iri.fromString "ab"),
             ("3", Iri.fromString "aba"),
             ("4", Iri.fromString "abba"),
             ("5", Iri.fromString "abc"),
             ("6", Iri.fromString "bbc"),
             ("7", Iri.fromString "bbf")]
             
    val realTable =
        makeTable 
            [("fruit", Iri.fromString "http://example.com/fruit/"),
             ("fruitloop", Iri.fromString "http://example.com/fruit/loop/"),
             ("veg", Iri.fromString "http://example.com/vegetable/"),
             ("", Iri.fromString "empty")]

    val nilTable =
        makeTable
            [("", Iri.fromString "")]
            
    fun abbrUnreal s = P.abbreviate (unrealTable, Iri.fromString s)
    fun abbrReal s = P.abbreviate (realTable, Iri.fromString s)

    fun abbrToString (SOME (ns, loc)) = ns ^ ":" ^ loc
      | abbrToString NONE = "(none)"

    fun tests () = [
        ("empty",
         fn () => null (P.enumerate P.empty)),
        ("empty-expand",
         fn () => P.expand (P.empty, "a:b") = Iri.fromString "a:b"),
        ("empty-abbreviate",
         fn () => P.abbreviate (P.empty, Iri.fromString "a:b") = NONE),
        ("add",
         fn () => P.enumerate (P.add (P.empty, ("a", Iri.fromString "b"))) = [("a", Iri.fromString "b")]),
        ("add-another",
         fn () => P.enumerate (P.add (P.add (P.empty, ("a", Iri.fromString "b")), ("aa", Iri.fromString "bb"))) =
                  [("a", Iri.fromString "b"), ("aa", Iri.fromString "bb")]), (*!!! compare unordered! *)
        ("replace",
         fn () => P.enumerate (P.add (P.add (P.empty, ("a", Iri.fromString "b")), ("a", Iri.fromString "bb"))) =
                  [("a", Iri.fromString "bb")]),
        ("expand",
         fn () =>
            checkPairs
                Iri.toString
                [(P.expand (realTable, "veg:aubergine"),
                  Iri.fromString "http://example.com/vegetable/aubergine"),
                 (P.expand (realTable, "fruit:banana:thing"),
                  Iri.fromString "http://example.com/fruit/banana:thing"),
                 (P.expand (realTable, "fruitloop:banana"),
                  Iri.fromString "http://example.com/fruit/loop/banana"),
                 (P.expand (realTable, ":banana"),
                  Iri.fromString "emptybanana"),
                 (P.expand (nilTable, "a.b"),
                  Iri.fromString "a.b"),
                 (P.expand (nilTable, ":a.b"),
                  Iri.fromString "a.b")]
        ),
        ("abbreviate",
         fn () =>
            checkPairs
                abbrToString
                [(abbrUnreal "a", SOME ("1", "")),
                 (abbrUnreal "aa", SOME ("1", "a")),
                 (abbrUnreal "abc", SOME ("5", "")),
                 (abbrUnreal "abd", SOME ("2", "d")),
                 (abbrUnreal "bbb", NONE),
                 (abbrUnreal "bbd", NONE),
                 (abbrUnreal "aba", SOME ("3", "")),
                 (abbrUnreal "abad", SOME ("3", "d"))]
        ),
        ("abbreviate-lifelike",
         fn () =>
            checkPairs
                abbrToString
                [(abbrReal "http://example.com/vegetable/aubergine", SOME ("veg", "aubergine")),
                 (abbrReal "http://example.com/fruit/banana:thing", SOME ("fruit", "banana:thing")),
                 (abbrReal "http://example.com/fruit/loop/banana", SOME ("fruitloop", "banana")),
                 (abbrReal "http://example.com/fruit/loop:banana", SOME ("fruit", "loop:banana")),
                 (abbrReal "http://example.com/fruit/", SOME ("fruit", "")),
                 (abbrReal "fruit", NONE),
                 (abbrReal "emptyfruit", SOME ("", "fruit"))]
        )
    ]

end

structure TestPrefix = TestPrefixFn(PrefixTable)
