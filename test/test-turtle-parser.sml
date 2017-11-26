
functor TestTurtleParserFn (P: RDF_PARSER) : TESTS = struct

    open TestSupport RdfTriple Prefix

    val name = "turtle-parser"
                  
    fun checkTriples str (P.PARSE_ERROR err) =
        (print ("\n--- Error in parsing \"" ^ str ^ "\": " ^ err ^ "\n");
         false)
      | checkTriples str (P.PARSED { prefixes, triples }) =
        if null triples then
            (print ("\n--- No triples obtained when parsing \"" ^ str ^ "\"\n");
             false)
        else true

    fun stringOfParseResult (P.PARSE_ERROR err) = ("(error: " ^ err ^ ")")
      | stringOfParseResult (P.PARSED { prefixes, triples }) =
        "prefixes [" ^ (String.concatWith ", " (map stringOfPrefix prefixes)) ^
        "], triples [" ^ (String.concatWith ", " (map stringOfTriple triples)) ^
        "]"
            
    fun checkParseFailed _ (P.PARSE_ERROR err) = true
      | checkParseFailed str res = 
        (print ("\n--- Parsing erroneously succeeded with input \"" ^ str
                ^ "\"\n    producing " ^ (stringOfParseResult res) ^ "\n");
         false)
            
    fun goodString str =
        checkTriples str (P.parse (NONE, TextIO.openString str))

    fun badString str =
        checkParseFailed str (P.parse (NONE, TextIO.openString str))
                
    fun goodStrings strs =
        List.all (fn x => x) (map goodString strs)
                
    fun badStrings strs =
        List.all (fn x => x) (map badString strs)

    fun goodFile f =
        let val s = TextIO.openIn f
            val result = checkTriples f (P.parse (NONE, s))
        in
            TextIO.closeIn s;
            result
        end
                 
    val testFileDir = "test/other"
                 
    val goodFileTests =
        map (fn f =>
                (f, fn () => goodFile (testFileDir ^ "/" ^ f ^ ".ttl")))
            [ "bnode-nested-2", "bnode-nested", "bnode", "boolean",
              "collections", "example1", "example2", "example3", "goblin",
              "iris", "numbers", "quoted" ]

    fun iriTriple (a,b,c) = (IRI (Iri.fromString a),
                              IRI (Iri.fromString b),
                              IRI (Iri.fromString c))

    fun checkIriTripleParse str p =
        checkPairs stringOfParseResult
                    [ (P.parse (NONE, TextIO.openString str),
                       P.PARSED p) ]

    fun tests () = [
        ("simple-string",     fn () => goodString "<a> <b> <c>."),
        ("simple-with-a",     fn () => goodString "<a> a <c>."),
        ("simple-with-space", fn () => goodString "<a> a <c> . "),
        ("empty-prefixes",    fn () => goodString "@prefix : <>. :a :b :c."),
        ("empty-prefix-sparql", fn () => goodString "prefix : <> :a :b :c."),
        ("nothing",           fn () => badString "."),
        ("no-dot",            fn () => badString "<a> a <b>"),
        ("ends-with-semi",    fn () => badString "<a> a <b>;"),
        ("lit-subject",       fn () => badString "\"a\" a <c>."),
        ("lit-predicate",     fn () => badString "<a> \"a\" <c>."),
        ("blank-predicate",   fn () => badString "<a> _:a <c>."),
        ("local-bad",         fn () => badStrings [
                                          ":a :b :c .",
                                          "@prefix : <>. :a:b :c .",
                                          "@prefix : <>. :a :b :.c .",
                                          "@prefix : <>. :a :b. :c .",
                                          "@prefix : <>. :a :b ::c" ]),
        ("local-slash-bad",   fn () => badStrings [
                                          "@prefix : <>. :\\a :b :c .",
                                          "@prefix : <>. :\\: :b :c .",
                                          "@prefix : <>. :\\  :b :c .",
                                          "@prefix : <>. :\\\\  :b :c .",
                                          "@prefix : <>. :\\< :b :c ." ]),
        ("local-u-bad",       fn () => badString
                                           "@prefix : <>.:abc :a\\u0062c :abc."),
        ("local-colon",
         fn () => checkIriTripleParse
                      "@prefix : <>. :a: :b :c."
                      { prefixes = [ ("", Iri.empty) ],
                        triples  = [ iriTriple ("a:", "b", "c") ] }
        ),
        ("local-dot",
         fn () => checkIriTripleParse
                      "@prefix : <>. :a.b :b..c :c.d."
                      { prefixes = [ ("", Iri.empty) ],
                        triples  = [ iriTriple ("a.b", "b..c", "c.d") ] }
        ),
        ("prefix-dot",
         fn () => checkIriTripleParse
                      "@prefix a.b: <a>. a.b:a a.b:b.c a.b:c:d ."
                      { prefixes = [ ("a.b", Iri.fromString "a") ],
                        triples  = [ iriTriple ("aa", "ab.c", "ac:d") ] }
        ),
        ("local-pc-escape",
         (* "%-encoded sequences are in the character range for
                IRIs and are explicitly allowed in local names. These
                appear as a '%' followed by two hex characters and
                represent that same sequence of three
                characters. These sequences are not decoded during
                processing." *)
         fn () => checkIriTripleParse
                      "@prefix : <>.:%61bc :a%62c :ab%63."
                      { prefixes = [ ( "", Iri.empty ) ],
                        triples  = [ iriTriple ("%61bc", "a%62c", "ab%63") ] }
        ),
        ("local-slash-escape-easy", (* escaped chars are not confusing ones *)
         fn () => checkIriTripleParse
                      "@prefix : <>.:\\~bc :a\\?c :ab\\$."
                      { prefixes = [ ( "", Iri.empty ) ],
                        triples  = [ iriTriple ("~bc", "a?c", "ab$") ] }
        ),
        ("local-slash-escape-tricky", (* escaped chars have other meanings *)
         fn () => checkIriTripleParse
                      "@prefix : <>.:\\% :a\\#c :ab\\(."
                      { prefixes = [ ( "", Iri.empty ) ],
                        triples  = [ iriTriple ("%", "a#c", "ab(") ] }
        )
    ] @ goodFileTests 
            
end

structure TestTurtleParser = TestTurtleParserFn(TurtleParser)

                                               
