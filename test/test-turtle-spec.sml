
functor TestTurtleSpecFn (P: RDF_PARSER) : TESTS = struct

    open TestSupport RdfTriple

    structure L = TurtleLoader (* for manifest in ttl format *)
    structure S = L.Store
                     
    val test_file_dir = "test/spec"
    val out_file_dir = "test/out"

    fun test_file filename = test_file_dir ^ "/" ^ filename
    fun temp_file filename = out_file_dir ^ "/" ^ filename
                     
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

    fun lines_of f =
        let fun lines' s =
                case TextIO.inputLine s of
                    SOME l => l :: lines' s
                  | NONE => []
            val s = TextIO.openIn f
            val lines = lines' s
        in
            TextIO.closeIn s;
            lines
        end

    fun scrub_blanks lines =
        let val blank_text = String.explode "{blank}"
            fun scrub [] = []
              | scrub (#"_"::rest) = scrub_in_blank rest
              | scrub (first::rest) = first :: scrub rest
            and scrub_in_blank [] = blank_text
              | scrub_in_blank (#" "::rest) = blank_text @ (#" " :: scrub rest)
              | scrub_in_blank (_::rest) = scrub_in_blank rest
        in
            map (String.implode o scrub o String.explode) lines
        end
            
    fun compare_ntriples f1 f2 =
        let val tidy_lines =
                (ListMergeSort.sort String.>) o scrub_blanks o lines_of
            val (l1, l2) = (tidy_lines f1, tidy_lines f2)
        in
            if l1 = l2 then true
            else
                (print ("\n--- Eval test output \"" ^ f1 ^
                       "\" differs from reference \"" ^ f2 ^ "\"\n");
                 print "    Output after sorting:\n";
                 print (String.concatWith "" (l1));
                 print "    Expected:\n";
                 print (String.concatWith "" (l2));
                 false)
        end
            
    fun good_conversion (base, fin, fout, reference) =
        let val instream = TextIO.openIn fin
            val outstream = TextIO.openOut fout
            open TurtleNTriplesConverter
            val result = convert base instream outstream
        in
            TextIO.closeIn instream;
            TextIO.closeOut outstream;
            case result of
                CONVERSION_ERROR e => (print ("\n--- Conversion failed: "^e^"\n");
                                       false)
              | CONVERTED => compare_ntriples fout reference
        end

    fun good_export (base, fin, action) =
        let open FileExtensionDrivenConverter
            val fout = temp_file action
            val out_ttl = fout ^ ".export.ttl"
            val out_ref = fout ^ ".ref.nt"
        in
            case convert base fin out_ttl of (* the Turtle export *)
                CONVERSION_ERROR e => (print ("\n--- Conversion to \"" ^ out_ttl ^
                                              "\" failed: " ^ e ^ "\n");
                                       false)
              | CONVERTED =>
                case convert base fin out_ref of (* and NTriples, as ref *)
                    CONVERSION_ERROR e => (print ("\n--- Conversion to \"" ^ out_ref
                                                  ^ "\" failed: " ^ e ^ "\n");
                                           false)
                  | CONVERTED => good_conversion (base, out_ttl, fout, out_ref)
        end
            
    val setup_count = ref 0
    fun setup_failed_test text =
        (setup_count := (!setup_count) + 1;
         ("setup-failed-" ^ (Int.toString (!setup_count)),
          fn () => (print ("\n--- Test setup failed: " ^ text ^ "\n"); false)))

    type testmeta = {
        name : string,
        comment : string,
        action : string,
        result : string
    }

    fun metadata_for s (test_node, _, _) : testmeta =
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

    (* Except for TURTLE_EXPORT, these correspond to the test types in
       the spec manifest. POSITIVE and NEGATIVE are syntax tests, EVAL
       tests require comparing the output against a reference.
       NEGATIVE_EVAL we treat like NEGATIVE for now. TURTLE_EXPORT is
       our own test type, which consists of exporting to Turtle,
       re-importing, and checking that the results are the same -- so
       it is a Turtle exporter test rather than a parser test. *)
            
    datatype test_type = POSITIVE | NEGATIVE | EVAL | NEGATIVE_EVAL | TURTLE_EXPORT

    fun test s tt triple =
        let fun eval_test ({ action, ... } : testmeta) POSITIVE =
                good_file (test_file action)

              | eval_test ({ action, ... } : testmeta) NEGATIVE = 
                bad_file (test_file action)
                                            
              | eval_test ({ action, ... } : testmeta) NEGATIVE_EVAL =
                bad_file (test_file action)

              | eval_test ({ action, result, ... } : testmeta) EVAL =
                (good_conversion (base_iri ^ action,
                                  test_file action,
                                  temp_file result,
                                  test_file result)
                 handle IO.Io { name, ... } =>
                        (print ("\n--- Failed to convert \"" ^ name ^ "\" to NTriples\n");
                         false))

              | eval_test ({ action, result, ... } : testmeta) TURTLE_EXPORT =
                (good_export (base_iri ^ action,
                              test_file action,
                              action)
                 handle IO.Io { name, ... } =>
                        (print ("\n--- Failed to convert \"" ^ name ^ "\" to Turtle\n");
                         false))
                                            
            val metadata = metadata_for s triple
        in
            if #name metadata = "" orelse #action metadata = ""
            then setup_failed_test
                     ("unable to retrieve test metadata for test: " ^
                      (string_of_node (#1 triple)))
            else
                let val test_name = 
                        if #comment metadata = "" then #name metadata
                        else (#name metadata) ^ ": \"" ^ (#comment metadata) ^ "\""
                in
                    (if tt = TURTLE_EXPORT then "export-" ^ test_name else test_name,
                     fn () => eval_test metadata tt)
                end
        end
                                                          
    fun tests_from_store s =
        let fun tests_of_type t = 
                S.match (s, (S.WILDCARD,
                             S.KNOWN (IRI (RdfStandardIRIs.iri_rdf_type)),
                             S.KNOWN (IRI (S.expand (s, "rdft:" ^ t)))))
        in
            map (test s POSITIVE) (tests_of_type "TestTurtlePositiveSyntax") @
            map (test s NEGATIVE) (tests_of_type "TestTurtleNegativeSyntax") @
            map (test s EVAL) (tests_of_type "TestTurtleEval") @
            map (test s NEGATIVE_EVAL) (tests_of_type "TestTurtleNegativeEval") @
            map (test s TURTLE_EXPORT) ((tests_of_type "TestTurtlePositiveSyntax") @
                                        (tests_of_type "TestTurtleEval"))
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

                                           
