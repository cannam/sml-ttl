
functor TestPrefixFn (P: PREFIX_TABLE) : TESTS = struct

    open TestTypes

    fun check_all converter pairs =
        case List.filter (op<>) pairs of
            [] => true
          | unequal =>
            (app (fn (a,b) =>
                     print ("*** Expected \"" ^ (converter b)
                            ^ "\", obtained \"" ^ (converter a) ^ "\"\n"))
                 unequal;
             false)
             
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
             
    val realish_table =
        make_table 
            [("fruit", "http://example.com/fruit/"),
             ("fruitloop", "http://example.com/fruit/loop/"),
             ("veg", "http://example.com/vegetable/"),
             ("", "empty")]

    fun id x = x
            
    val tests =
        ("prefix",
         [
           ("empty",
            fn () => null (P.enumerate P.empty)),
           ("empty-expand",
            fn () => P.expand (P.empty, "a:b") = Iri.fromString "a:b"),
           ("empty-abbreviate",
            fn () => P.abbreviate (P.empty, Iri.fromString "a:b") = "a:b"),
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
                   [(P.expand (realish_table, "veg:aubergine"),
                     Iri.fromString "http://example.com/vegetable/aubergine"),
                    (P.expand (realish_table, "fruit:banana:thing"),
                     Iri.fromString "http://example.com/fruit/banana:thing"),
                    (P.expand (realish_table, "fruitloop:banana"),
                     Iri.fromString "http://example.com/fruit/loop/banana")]
           ),
           ("abbreviate",
            fn () =>
               check_all
                   String.toString
                   [(P.abbreviate (unreal_table, Iri.fromString "a"), "1:"),
                    (P.abbreviate (unreal_table, Iri.fromString "aa"), "1:a"),
                    (P.abbreviate (unreal_table, Iri.fromString "abc"), "5:"),
                    (P.abbreviate (unreal_table, Iri.fromString "abd"), "2:d"),
                    (P.abbreviate (unreal_table, Iri.fromString "bbb"), "bbb"),
                    (P.abbreviate (unreal_table, Iri.fromString "bbd"), "bbd"),
                    (P.abbreviate (unreal_table, Iri.fromString "aba"), "3:"),
                    (P.abbreviate (unreal_table, Iri.fromString "abad"), "3:d")]
           ),
           ("abbreviate-lifelike",
            fn () =>
               check_all
                   String.toString
                   [(P.abbreviate (realish_table, Iri.fromString "http://example.com/vegetable/aubergine"), "veg:aubergine"),
                    (P.abbreviate (realish_table, Iri.fromString "http://example.com/fruit/banana:thing"), "fruit:banana:thing"),
                    (P.abbreviate (realish_table, Iri.fromString "http://example.com/fruit/loop/banana"), "fruitloop:banana"),
                    (P.abbreviate (realish_table, Iri.fromString "http://example.com/fruit/loop:banana"), "fruit:loop:banana"),
                    (P.abbreviate (realish_table, Iri.fromString "http://example.com/fruit/"), "fruit:"),
                    (P.abbreviate (realish_table, Iri.fromString "fruit"), "fruit"),
                    (P.abbreviate (realish_table, Iri.fromString "emptyfruit"), ":fruit")]
           )
         ]
        )
             
end
                                                    
structure TestPrefix = TestPrefixFn(PrefixTable)
