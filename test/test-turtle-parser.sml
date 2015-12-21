
functor TestTurtleParserFn (P: RDF_PARSER) : TESTS = struct

    open TestSupport RdfTriple

    fun check_triples str (P.PARSE_ERROR err) =
        (print ("\n--- Error in parsing \"" ^ str ^ "\": " ^ err ^ "\n");
         false)
      | check_triples str (P.PARSED { prefixes, triples }) =
        if null triples then
            (print ("\n--- No triples obtained when parsing \"" ^ str ^ "\"\n");
             false)
        else true

    fun string_of_parse_result (P.PARSE_ERROR err) = ("(error: " ^ err ^ ")")
      | string_of_parse_result (P.PARSED { prefixes, triples }) =
        "prefixes [" ^ (String.concatWith ", " (map string_of_prefix prefixes)) ^
        "], triples [" ^ (String.concatWith ", " (map string_of_triple triples)) ^
        "]"
            
    fun check_parse_failed _ (P.PARSE_ERROR err) = true
      | check_parse_failed str res = 
        (print ("\n--- Parsing erroneously succeeded with input \"" ^ str
                ^ "\"\n    producing " ^ (string_of_parse_result res) ^ "\n");
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

    fun iri_triple (a,b,c) = (IRI (Iri.fromString a),
                              IRI (Iri.fromString b),
                              IRI (Iri.fromString c))

    fun check_iri_triple_parse str p =
        check_all string_of_parse_result
                  [ (P.parse "" (TextIO.openString str),
                     P.PARSED p) ]
                                 
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
                                            "@prefix : <>. :\\< :b :c ." ]),
          ("local-colon",
           fn () => check_iri_triple_parse
                        "@prefix : <>. :a: :b :c."
                        { prefixes = [ ("", "") ],
                          triples  = [ iri_triple ("a:", "b", "c") ] }
          ),
          ("local-dot",
           fn () => check_iri_triple_parse
                        "@prefix : <>. :a.b :b..c :c.d."
                        { prefixes = [ ("", "") ],
                          triples  = [ iri_triple ("a.b", "b..c", "c.d") ] }
          ),
          ("prefix-dot",
           fn () => check_iri_triple_parse
                        "@prefix a.b: <a>. a.b:a a.b:b.c a.b:c:d ."
                        { prefixes = [ ("a.b", "a") ],
                          triples  = [ iri_triple ("aa", "ab.c", "ac:d") ] }
          ),
          ("local-u-escape",
           fn () => check_iri_triple_parse
                        "@prefix : <>.:\u0061bc :a\u0062c :ab\u0063."
                        { prefixes = [ ( "", "" ) ],
                          triples  = [ iri_triple ("abc", "abc", "abc") ] }
          ),
          ("local-pc-escape",
            (* "%-encoded sequences are in the character range for
                IRIs and are explicitly allowed in local names. These
                appear as a '%' followed by two hex characters and
                represent that same sequence of three
                characters. These sequences are not decoded during
                processing." *)
           fn () => check_iri_triple_parse
                        "@prefix : <>.:%61bc :a%62c :ab%63."
                        { prefixes = [ ( "", "" ) ],
                          triples  = [ iri_triple ("%61bc", "a%62c", "ab%63") ] }
          ),
          ("local-slash-escape-easy", (* escaped chars are not confusing ones *)
           fn () => check_iri_triple_parse
                        "@prefix : <>.:\\~bc :a\\?c :ab\\$."
                        { prefixes = [ ( "", "" ) ],
                          triples  = [ iri_triple ("~bc", "a?c", "ab$") ] }
          ),
          ("local-slash-escape-tricky", (* escaped chars have other meanings *)
           fn () => check_iri_triple_parse
                        "@prefix : <>.:\\% :a\\#c :ab\\(."
                        { prefixes = [ ( "", "" ) ],
                          triples  = [ iri_triple ("%", "a#c", "ab(") ] }
          )
        ]
            @ good_file_tests 
        )

end

structure TestTurtleParser = TestTurtleParserFn(TurtleParser)

                                               
