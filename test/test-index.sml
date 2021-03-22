
signature TEST_INDEX_ARG = sig

    structure IX : INDEX
    
end

functor TestIndexFn (Arg : TEST_INDEX_ARG) :> TESTS = struct

    open TestSupport

    structure IX = Arg.IX
    structure P = IndexPickerFn(IX)

    fun checkSets converter greater (a, b) =
        checkLists converter
                    (ListMergeSort.sort greater a,
                     ListMergeSort.sort greater b)

    type test = string * (unit -> bool)
                             
    val name = "index"

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

    val indexNames = [ "spo", "pos", "ops", "sop", "pso", "osp" ]

    fun match (index, pattern) =
        IX.foldlMatch (fn (t, acc) => t :: acc)
                       []
                       (index, pattern)

    fun enumerate index =
        match (index, makeTestPattern "***")

    fun newIndexFor name =
        IX.new (IX.orderOfName name)

    fun buildIndex name templates =
        List.foldl (fn (t, ix) => IX.add (ix, makeTestTriple t))
                   (newIndexFor name)
                   templates

    fun checkTripleLists (a, b) =
        checkSets RdfTriple.stringOfTriple
                   (fn (t1, t2) => RdfTriple.compare (t1, t2) = GREATER)
                   (a, b)

    fun indexTests name = [

        (name ^ "-empty",
         fn () => checkTripleLists
                      (enumerate (newIndexFor name),
                       [])),
        
        (name ^ "-simple",
         fn () => checkTripleLists
                      (enumerate (buildIndex name ["aaa"]),
                       [makeTestTriple "aaa"])),

        (name ^ "-duplicate",
         fn () => checkTripleLists
                      (enumerate (buildIndex name ["aaa", "aaa"]),
                       [makeTestTriple "aaa"])),

        (name ^ "-common",
         fn () => checkTripleLists
                      (enumerate (buildIndex name ["aaa", "aab"]),
                       [makeTestTriple "aab", makeTestTriple "aaa"])),

        (name ^ "-diverging",
         fn () => checkTripleLists
                      (enumerate (buildIndex name ["aaa", "aab", "abc", "bba"]),
                       [makeTestTriple "abc",
                        makeTestTriple "aaa",
                        makeTestTriple "aab",
                        makeTestTriple "bba"])),
        
        (name ^ "-contains",
         fn () =>
            let val ix = buildIndex name ["aaa", "aab", "abc", "bba"]
            in
                checkPairs Bool.toString
                          [(IX.contains (ix, makeTestTriple "aaa"), true),
                           (IX.contains (ix, makeTestTriple "aab"), true),
                           (IX.contains (ix, makeTestTriple "abc"), true),
                           (IX.contains (ix, makeTestTriple "bba"), true),
                           (IX.contains (ix, makeTestTriple "bca"), false)]
            end),
        
        (name ^ "-remove",
         fn () =>
            let val ix = buildIndex name ["aaa", "aab", "abc", "bba"]
                val ixR = IX.remove (ix, makeTestTriple "aaa")
                val ixRr = IX.remove (IX.remove (ix, makeTestTriple "aaa"),
                                       makeTestTriple "aaa")
            in
                checkPairs Bool.toString
                          [(IX.contains (ix, makeTestTriple "aaa"), true),
                           (IX.contains (ixR, makeTestTriple "aaa"), false),
                           (IX.contains (ixRr, makeTestTriple "aaa"), false),
                           (IX.contains (ixR, makeTestTriple "abc"), true),
                           (IX.contains (ixRr, makeTestTriple "bba"), true)]
            end),

        (name ^ "-match",
         fn () =>
            let val ix = buildIndex name ["aaa", "aab", "abc", "bba"]
            in
                checkTripleLists
                    (match (ix, makeTestPattern "***"),
                     enumerate ix)
                andalso
                checkTripleLists
                    (match (ix, makeTestPattern "**a"),
                     [makeTestTriple "aaa", makeTestTriple "bba"])
            end)
    ]
                          
    val namedTests = List.concat (map indexTests indexNames)

    fun checkPickedIndex ixs pattern names =
        (* Check that the index picked from ixs for a pattern is one
           of the expected set given by names *)
        let val ix = P.pickIndex (ixs, makeTestPattern pattern)
        in
            case List.find (fn n => (IX.name ix = n)) names of
                SOME _ => true
              | NONE => (report (fn x => x)
                                ("one of: " ^ (String.concatWith ", " names),
                                 IX.name ix);
                         false)
        end
                                  
    fun testChooseIndexFromAll () =
        let val ixs = List.map newIndexFor indexNames
        in
            checkPickedIndex ixs "aa*" ["spo", "pso"] andalso
            checkPickedIndex ixs "a**" ["spo", "sop"] andalso
            checkPickedIndex ixs "aaa" indexNames andalso
            checkPickedIndex ixs "a*a" ["sop", "osp"] andalso
            checkPickedIndex ixs "***" indexNames andalso
            checkPickedIndex ixs "*a*" ["pso", "pos"]
        end

    fun testChooseIndexFromSubset () =
        let val ixs = List.map newIndexFor [ "spo", "pos", "ops" ]
        in
            checkPickedIndex ixs "aa*" ["spo"] andalso
            checkPickedIndex ixs "a**" ["spo"] andalso
            checkPickedIndex ixs "aaa" ["spo", "pos", "ops"] andalso
            checkPickedIndex ixs "a*a" ["ops", "spo"] andalso
            checkPickedIndex ixs "***" ["spo", "pos", "ops"] andalso
            checkPickedIndex ixs "*a*" ["pos"]
        end
                                  
    fun tests () =
        (namedTests @
         [
           ("choose-index-from-all", testChooseIndexFromAll),
           ("choose-index-from-subset", testChooseIndexFromSubset)
        ])
             
end

structure TestIndex = TestIndexFn(struct
                                   structure IX = Index
                                   end)
                                 
