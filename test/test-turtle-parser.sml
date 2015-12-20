
functor TestTurtleParserFn (P: RDF_PARSER) : TESTS = struct

    open TestSupport

    fun check_triples str (P.PARSE_ERROR err) =
        (print ("--- Error in parsing \"" ^ str ^ "\": " ^ err ^ "\n");
         false)
      | check_triples str (P.PARSED { prefixes, triples }) =
        if null triples then
            (print ("--- No triples obtained when parsing \"" ^ str ^ "\"\n");
             false)
        else true

    fun check_parse_failed _ (P.PARSE_ERROR err) = true
      | check_parse_failed str (P.PARSED { prefixes, triples }) = 
        (print ("--- Parsing erroneously succeeded with input \"" ^ str
                ^ "\"\n    producing " ^ (Int.toString (length prefixes)) ^
                " prefix(es) and " ^ (Int.toString (length triples)) ^
                " triple(s)\n");
         false)
             
    fun good_string str =
        check_triples str (P.parse "" (TextIO.openString str))

    fun bad_string str =
        check_parse_failed str (P.parse "" (TextIO.openString str))
                
    fun good_strings strs =
        List.all (fn x => x) (map good_string strs)
                
    fun bad_strings strs =
        List.all (fn x => x) (map bad_string strs)

    fun good_file f =
        let val s = TextIO.openIn f
            val result = check_triples f (P.parse "" s)
        in
            TextIO.closeIn s;
            result
        end
                 
    val test_file_dir = "test/other"
                 
    val good_file_tests =
        map (fn f =>
                (f, fn () => good_file (test_file_dir ^ "/" ^ f ^ ".ttl")))
            [ "bnode-nested-2", "bnode-nested", "bnode", "boolean",
              "collections", "example1", "example2", "example3", "goblin",
              "iris", "numbers", "quoted" ]
                 
    val tests = (
        "turtle-parser",
        [
          ("simple-string",     fn () => good_string "<a> <b> <c>."),
          ("simple-with-a",     fn () => good_string "<a> a <c>."),
          ("simple-with-space", fn () => good_string "<a> a <c> . "),
          ("empty-prefixes",    fn () => good_string "@prefix : <>. :a :b :c."),
          ("nothing",           fn () => bad_string "."),
          ("no-dot",            fn () => bad_string "<a> a <b>"),
          ("ends-with-semi",    fn () => bad_string "<a> a <b>;"),
          ("lit-subject",       fn () => bad_string "\"a\" a <c>."),
          ("lit-predicate",     fn () => bad_string "<a> \"a\" <c>."),
          ("blank-predicate",   fn () => bad_string "<a> _:a <c>."),
          ("local-bad",         fn () => bad_strings [
                                            ":a :b :c .",
                                            "@prefix : <>. :a:b :c .",
                                            "@prefix : <>. :a :b :.c .",
                                            "@prefix : <>. :a :b. :c .",
                                            "@prefix : <>. :a :b ::c" ]),
          ("local-slash-bad",   fn () => bad_strings [
                                            "@prefix : <>. :\\a :b :c .",
                                            "@prefix : <>. :\\: :b :c .",
                                            "@prefix : <>. :\\  :b :c .",
                                            "@prefix : <>. :\\\\  :b :c .",
                                            "@prefix : <>. :\\< :b :c ." ])
        ]
            @ good_file_tests 
        )

end

structure TestTurtleParser = TestTurtleParserFn(TurtleParser)

                                               
