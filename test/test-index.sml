
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

    fun enumerate index =
        IX.fold_match (fn (t, acc) => t :: acc)
                      []
                      (index, (make_test_wildcard "***"))

    fun new_index_for name =
        IX.new (IX.order_of_name name)

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

    val check_triple_lists = check_lists RdfTriple.string_of_triple
                                              
    fun index_tests name = [

        (name ^ "-empty",
         fn () => check_triple_lists
                      (enumerate (new_index_for name),
                       [])),
        
        (name ^ "-simple",
         fn () => check_triple_lists
                      (enumerate (IX.add (new_index_for name,
                                          make_test_triple "aaa")),
                       [make_test_triple "aaa"]))

            (*!!! + more! *)
    ]
                          
    fun test_remind_myself_to_implement_something () = false

    val named_tests = List.concat (map index_tests index_names)
                                                           
    fun tests () =
        (named_tests @
         [
           ("remind-myself-to-implement-something",
            test_remind_myself_to_implement_something)
        ])
             
end

structure TestIndex = TestIndexFn(struct
                                   structure IX = Index
                                   structure P = IndexPicker
                                   end)
                                 
