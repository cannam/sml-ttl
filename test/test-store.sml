
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
    fun known i = SOME (RdfNode.IRI (Iri.fromString i))
    val wildcard = NONE

    fun makeTestTriple "aaa" = (iri "fred", iri "loves", iri "cheese")
      | makeTestTriple "aab" = (iri "fred", iri "loves", iri "fruit")
      | makeTestTriple "abc" = (iri "fred", iri "hates", iri "vinegar")
      | makeTestTriple "bba" = (iri "jodie", iri "hates", iri "cheese")
      | makeTestTriple "bca" = (iri "jodie", iri "tolerates", iri "fred")
      | makeTestTriple x = raise Fail ("Unsupported triple template " ^ x)
                       
    fun makeTestPattern "***" = (wildcard, wildcard, wildcard)
      | makeTestPattern "aaa" = (known "fred", known "loves", known "cheese")
      | makeTestPattern "aa*" = (known "fred", known "loves", wildcard)
      | makeTestPattern "a**" = (known "fred", wildcard, wildcard)
      | makeTestPattern "*a*" = (wildcard, known "loves", wildcard)
      | makeTestPattern "a*a" = (known "fred", wildcard, known "cheese")
      | makeTestPattern "**a" = (wildcard, wildcard, known "cheese")
      | makeTestPattern x = raise Fail ("Unsupported wildcard template " ^ x)

    fun buildStore name templates =
        List.foldl (fn (t, st) => S.add (st, makeTestTriple t))
                   S.empty
                   templates

    fun checkTripleLists (a, b) =
        checkSets RdfTriple.stringOfTriple
                   (fn (t1, t2) => RdfTriple.compare (t1, t2) = GREATER)
                   (a, b)

    fun tests () = [

        ("store-empty",
         fn () => checkTripleLists
                      (S.enumerate S.empty,
                       [])),
        
        ("store-simple",
         fn () => checkTripleLists
                      (S.enumerate (buildStore name ["aaa"]),
                       [makeTestTriple "aaa"])),

        ("store-duplicate",
         fn () => checkTripleLists
                      (S.enumerate (buildStore name ["aaa", "aaa"]),
                       [makeTestTriple "aaa"])),

        ("store-common",
         fn () => checkTripleLists
                      (S.enumerate (buildStore name ["aaa", "aab"]),
                       [makeTestTriple "aab", makeTestTriple "aaa"])),

        ("store-diverging",
         fn () => checkTripleLists
                      (S.enumerate (buildStore name ["aaa", "aab", "abc", "bba"]),
                       [makeTestTriple "abc",
                        makeTestTriple "aaa",
                        makeTestTriple "aab",
                        makeTestTriple "bba"])),
        
        ("store-contains",
         fn () =>
            let val st = buildStore name ["aaa", "aab", "abc", "bba"]
            in
                checkPairs Bool.toString
                          [(S.contains (st, makeTestTriple "aaa"), true),
                           (S.contains (st, makeTestTriple "aab"), true),
                           (S.contains (st, makeTestTriple "abc"), true),
                           (S.contains (st, makeTestTriple "bba"), true),
                           (S.contains (st, makeTestTriple "bca"), false)]
            end),
        
        ("store-remove",
         fn () =>
            let val st = buildStore name ["aaa", "aab", "abc", "bba"]
                val stR = S.remove (st, makeTestTriple "aaa")
                val stRr = S.remove (S.remove (st, makeTestTriple "aaa"),
                                       makeTestTriple "aaa")
            in
                checkPairs Bool.toString
                          [(S.contains (st, makeTestTriple "aaa"), true),
                           (S.contains (stR, makeTestTriple "aaa"), false),
                           (S.contains (stRr, makeTestTriple "aaa"), false),
                           (S.contains (stR, makeTestTriple "abc"), true),
                           (S.contains (stRr, makeTestTriple "bba"), true)]
            end),

        ("store-match",
         fn () =>
            let val st = buildStore name ["aaa", "aab", "abc", "bba"]
            in
                checkTripleLists
                    (S.match (st, makeTestPattern "***"),
                     S.enumerate st)
                andalso
                checkTripleLists
                    (S.match (st, makeTestPattern "**a"),
                     [makeTestTriple "aaa", makeTestTriple "bba"])
            end)
    ]
             
end

structure TestStore = TestStoreFn(struct
                                   structure S = Store
                                   end)
                                 
