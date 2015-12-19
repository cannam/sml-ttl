
functor TestPrefixFn (P: PREFIX_TABLE) : TESTS = struct

    open TestTypes

    val test_table =
        let val pairs = 
                [("fruit", "http://example.com/fruit/"),
                 ("fruitloop", "http://example.com/fruit/loop/"),
                 ("veg", "http://example.com/vegetable/")]
        in
            foldl (fn ((p, e), t) => P.add (t, p, e)) P.empty pairs
        end

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
                     [("a","bb")]) ,
           ("expand",
            fn () =>
               List.all id
                        [P.expand (test_table, "veg:aubergine") =
                         Iri.fromString "http://example.com/vegetable/aubergine",
                         P.expand (test_table, "fruit:banana:thing") =
                         Iri.fromString "http://example.com/fruit/banana:thing",
                         P.expand (test_table, "fruitloop:banana") =
                         Iri.fromString "http://example.com/fruit/loop/banana"]
           )
         ]
    )
             
end
                                                    
structure TestPrefix = TestPrefixFn(PrefixTable)
