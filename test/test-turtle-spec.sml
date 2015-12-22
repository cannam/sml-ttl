
functor TestTurtleSpecFn (P: RDF_PARSER) : TESTS = struct

    open TestSupport RdfTriple

    structure L = TurtleLoader (* for manifest in ttl format *)
    structure S = L.Store
                     
    val test_file_dir = "test/spec"
    val out_file_dir = "test/out"

    fun test_file filename = test_file_dir ^ "/" ^ filename
                     
    val base_iri = "http://example/base/"

    (*!!! common with test-turtle-parser *)
            
    fun check_parse_succeeded _ (P.PARSED whatever) = true
      | check_parse_succeeded str (P.PARSE_ERROR err) =
        (print ("\n--- Error in parsing \"" ^ str ^ "\": " ^ err ^ "\n");
         false)
            
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

    fun good_file f =
        let val s = TextIO.openIn f
            val result = check_parse_succeeded f (P.parse "" s)
        in
            TextIO.closeIn s;
            result
        end

    fun bad_file f =
        let val s = TextIO.openIn f
            val result = check_parse_failed f (P.parse "" s)
        in
            TextIO.closeIn s;
            result
        end

    fun good_conversion (fin, fout) =
        false
            
    val setup_count = ref 0
    fun setup_failed_test text =
        (setup_count := (!setup_count) + 1;
         ("setup-failed-" ^ (Int.toString (!setup_count)),
          fn () => (print ("\n--- Test setup failed: " ^ text ^ "\n"); false)))
            
    fun metadata_for s (test_node, _, _) =
        let fun text_of [(_, _, S.LITERAL obj)] = #value obj
              | text_of [(_, _, S.IRI iri)] = Iri.toString iri
              | text_of anything_else = ""

            and property_text property =
                text_of
                    (S.match (s, (S.KNOWN test_node,
                                  S.KNOWN (IRI (S.expand (s, property))),
                                  S.WILDCARD)))

            val name = property_text "mf:name"
            val comment = property_text "rdfs:comment"
            val action = property_text "mf:action"
            val result = property_text "mf:result"
        in
            { name = name, comment = comment, action = action, result = result }
        end

    datatype test_type = POSITIVE | NEGATIVE | EVAL | NEGATIVE_EVAL

    fun test s tt triple =
        let fun eval_test { action, ... } POSITIVE =
                good_file (test_file action)

              | eval_test { action, ... } NEGATIVE = 
                bad_file (test_file action)

              | eval_test { action, result, ... } EVAL =
                good_conversion (test_file action, test_file result)
                                            
              | eval_test { action, ... } NEGATIVE_EVAL =
                bad_file (test_file action)
                                            
            val metadata = metadata_for s triple
        in
            if #name metadata = "" orelse #action metadata = ""
            then setup_failed_test
                     ("unable to retrieve test metadata for test: " ^
                      (string_of_node (#1 triple)))
            else
                (if #comment metadata = "" then #name metadata
                 else (#name metadata) ^ ": \"" ^ (#comment metadata) ^ "\"",
                 (fn () => eval_test metadata tt))
        end
                                                          
    fun tests_from_store s =
        let fun tests_of_type t = 
                S.match (s, (S.WILDCARD,
                             S.KNOWN (IRI (S.expand (s, "a"))),
                             S.KNOWN (IRI (S.expand (s, "rdft:" ^ t)))))
        in
            map (test s POSITIVE) (tests_of_type "TestTurtlePositiveSyntax") @
            map (test s NEGATIVE) (tests_of_type "TestTurtleNegativeSyntax") @
            map (test s EVAL) (tests_of_type "TestTurtleEval") @
            map (test s NEGATIVE_EVAL) (tests_of_type "TestTurtleNegativeEval")
        end
            
    fun tests_from_manifest name =
        case L.load_file_as_new_store "" (test_file name)
             handle IO.Io { name, ... } =>
                    L.LOAD_ERROR ("failed to open file \"" ^ name ^ "\"")
         of
            L.LOAD_ERROR err => [setup_failed_test err]
          | L.OK store =>
            let val n = length (S.enumerate store) in
                if n < 100 
                then [setup_failed_test
                          ("too few (" ^ (Int.toString n) ^
                           ") triples in manifest (load problem?)")]
                else tests_from_store store
            end
                       
    val tests = (
        "turtle-spec",
        tests_from_manifest "manifest.ttl"
    )
                     
end

structure TestTurtleSpec = TestTurtleSpecFn(TurtleParser)

                                           
