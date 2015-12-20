
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
             
    val real_table =
        make_table 
            [("fruit", "http://example.com/fruit/"),
             ("fruitloop", "http://example.com/fruit/loop/"),
             ("veg", "http://example.com/vegetable/"),
             ("", "empty")]

    fun abbr_unreal s = P.abbreviate (unreal_table, Iri.fromString s)
    fun abbr_real s = P.abbreviate (real_table, Iri.fromString s)
                   
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
                   [(P.expand (real_table, "veg:aubergine"),
                     Iri.fromString "http://example.com/vegetable/aubergine"),
                    (P.expand (real_table, "fruit:banana:thing"),
                     Iri.fromString "http://example.com/fruit/banana:thing"),
                    (P.expand (real_table, "fruitloop:banana"),
                     Iri.fromString "http://example.com/fruit/loop/banana")]
           ),
           ("abbreviate",
            fn () =>
               check_all
                   String.toString
                   [(abbr_unreal "a", "1:"),
                    (abbr_unreal "aa", "1:a"),
                    (abbr_unreal "abc", "5:"),
                    (abbr_unreal "abd", "2:d"),
                    (abbr_unreal "bbb", "bbb"),
                    (abbr_unreal "bbd", "bbd"),
                    (abbr_unreal "aba", "3:"),
                    (abbr_unreal "abad", "3:d")]
           ),
           ("abbreviate-lifelike",
            fn () =>
               check_all
                   String.toString
                   [(abbr_real "http://example.com/vegetable/aubergine", "veg:aubergine"),
                    (abbr_real "http://example.com/fruit/banana:thing", "fruit:banana:thing"),
                    (abbr_real "http://example.com/fruit/loop/banana", "fruitloop:banana"),
                    (abbr_real "http://example.com/fruit/loop:banana", "fruit:loop:banana"),
                    (abbr_real "http://example.com/fruit/", "fruit:"),
                    (abbr_real "fruit", "fruit"),
                    (abbr_real "emptyfruit", ":fruit")]
           )
         ]
        )
             
end
                                                    
structure TestPrefix = TestPrefixFn(PrefixTable)
