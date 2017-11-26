
functor TestTurtleSpecFn (P: RDF_PARSER) : TESTS = struct

    open TestSupport RdfNode RdfTriple Prefix

    val name = "turtle-spec"

    structure L = TurtleLoader (* for manifest in ttl format *)
    structure S = Store
                     
    val testFileDir = "test/spec"
    val outFileDir = "test/out"

    fun testFile filename = testFileDir ^ "/" ^ filename
    fun tempFile filename = outFileDir ^ "/" ^ filename
                     
    val baseIri = "http://example/base/"

    (*!!! common with test-turtle-parser *)
            
    fun checkParseSucceeded _ (P.PARSED whatever) = true
      | checkParseSucceeded str (P.PARSE_ERROR err) =
        (print ("\n--- Error in parsing \"" ^ str ^ "\": " ^ err ^ "\n");
         false)
            
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

    fun goodFile f =
        let val s = TextIO.openIn f
            val result = checkParseSucceeded f (P.parse (NONE, s))
        in
            TextIO.closeIn s;
            result
        end

    fun badFile f =
        let val s = TextIO.openIn f
            val result = checkParseFailed f (P.parse (NONE, s))
        in
            TextIO.closeIn s;
            result
        end

    fun linesOf f =
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

    fun scrubBlanks lines =
        let val blankText = String.explode "{blank}"
            fun scrub [] = []
              | scrub (#"_"::rest) = scrubInBlank rest
              | scrub (first::rest) = first :: scrub rest
            and scrubInBlank [] = blankText
              | scrubInBlank (#" "::rest) = blankText @ (#" " :: scrub rest)
              | scrubInBlank (_::rest) = scrubInBlank rest
        in
            map (String.implode o scrub o String.explode) lines
        end
            
    fun compareNtriples f1 f2 =
        let val tidyLines =
                (ListMergeSort.sort String.>) o scrubBlanks o linesOf
            val (l1, l2) = (tidyLines f1, tidyLines f2)
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
            
    fun goodConversion (base, fin, fout, reference) =
        let val instream = TextIO.openIn fin
            val outstream = TextIO.openOut fout
            open TurtleNTriplesConverter
            val result = convert (base, instream) (base, outstream)
        in
            TextIO.closeIn instream;
            TextIO.closeOut outstream;
            case result of
                CONVERSION_ERROR e => (print ("\n--- Conversion failed: "^e^"\n");
                                       false)
              | CONVERTED => compareNtriples fout reference
        end

    fun goodExport (base, fin, action) =
        let open FileExtensionDrivenConverter
            val fout = tempFile action
            val outTtl = fout ^ ".export.ttl"
            val outRef = fout ^ ".ref.nt"
            fun bailTtl err = (print ("\n--- Conversion to \"" ^ outTtl ^
                                       "\" failed: " ^ err ^ "\n");
                                false)
            fun bailRef err = (print ("\n--- Conversion to \"" ^ outRef ^
                                       "\" failed: " ^ err ^ "\n");
                                false)
        in
            case convert (base, fin) (base, outTtl) of (* the Turtle export *)
                INPUT_FORMAT_NOT_SUPPORTED => bailTtl "Input format not supported"
              | OUTPUT_FORMAT_NOT_SUPPORTED => bailTtl "Output format not supported"
              | SYSTEM_ERROR err => bailTtl err
              | CONVERSION_ERROR err => bailTtl err
              | CONVERTED =>
                case convert (base, fin) (base, outRef) of (* and NTriples, as ref *)
                    INPUT_FORMAT_NOT_SUPPORTED => bailRef "Input format not supported"
                  | OUTPUT_FORMAT_NOT_SUPPORTED => bailRef "Output format not supported"
                  | SYSTEM_ERROR err => bailRef err
                  | CONVERSION_ERROR err => bailRef err
                  | CONVERTED => goodConversion (base, outTtl, fout, outRef)
        end
            
    val setupCount = ref 0
    fun setupFailedTest text =
        (setupCount := (!setupCount) + 1;
         ("setup-failed-" ^ (Int.toString (!setupCount)),
          fn () => (print ("\n--- Test setup failed: " ^ text ^ "\n"); false)))

    type testmeta = {
        name : string,
        comment : string,
        action : string,
        result : string
    }

    fun metadataFor s (testNode, _, _) : testmeta =
        let fun textOf [(_, _, S.LITERAL obj)] = #value obj
              | textOf [(_, _, S.IRI iri)] = Iri.toString iri
              | textOf anythingElse = ""

            and propertyText property =
                textOf
                    (S.match (s, (SOME testNode,
                                  SOME (IRI (S.expand (s, property))),
                                  NONE)))

            val name = propertyText "mf:name"
            val comment = propertyText "rdfs:comment"
            val action = propertyText "mf:action"
            val result = propertyText "mf:result"
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
            
    datatype testType = POSITIVE | NEGATIVE | EVAL | NEGATIVE_EVAL | TURTLE_EXPORT

    fun test s tt triple =
        let fun evalTest ({ action, ... } : testmeta) POSITIVE =
                goodFile (testFile action)

              | evalTest ({ action, ... } : testmeta) NEGATIVE = 
                badFile (testFile action)
                                            
              | evalTest ({ action, ... } : testmeta) NEGATIVE_EVAL =
                badFile (testFile action)

              | evalTest ({ action, result, ... } : testmeta) EVAL =
                (goodConversion (SOME (Iri.fromString (baseIri ^ action)),
                                  testFile action,
                                  tempFile result,
                                  testFile result)
                 handle IO.Io { name, ... } =>
                        (print ("\n--- Failed to convert \"" ^ name ^ "\" to NTriples\n");
                         false))

              | evalTest ({ action, result, ... } : testmeta) TURTLE_EXPORT =
                (goodExport (SOME (Iri.fromString (baseIri ^ action)),
                              testFile action,
                              action)
                 handle IO.Io { name, ... } =>
                        (print ("\n--- Failed to convert \"" ^ name ^ "\" to Turtle\n");
                         false))
                                            
            val metadata = metadataFor s triple
        in
            if #name metadata = "" orelse #action metadata = ""
            then setupFailedTest
                     ("unable to retrieve test metadata for test: " ^
                      (stringOfNode (#1 triple)))
            else
                let val testName = 
                        if #comment metadata = "" then #name metadata
                        else (#name metadata) ^ ": \"" ^ (#comment metadata) ^ "\""
                in
                    (if tt = TURTLE_EXPORT then "export-" ^ testName else testName,
                     fn () => evalTest metadata tt)
                end
        end
                                                          
    fun testsFromStore s =
        let fun testsOfType t = 
                S.match (s, (NONE,
                             SOME (IRI (RdfStandardIRIs.iriRdfType)),
                             SOME (IRI (S.expand (s, "rdft:" ^ t)))))
        in
            map (test s POSITIVE) (testsOfType "TestTurtlePositiveSyntax") @
            map (test s NEGATIVE) (testsOfType "TestTurtleNegativeSyntax") @
            map (test s EVAL) (testsOfType "TestTurtleEval") @
            map (test s NEGATIVE_EVAL) (testsOfType "TestTurtleNegativeEval") @
            map (test s TURTLE_EXPORT) ((testsOfType "TestTurtlePositiveSyntax") @
                                        (testsOfType "TestTurtleEval"))
        end
            
    fun testsFromManifest name =
        case L.loadFileAsNewStore (NONE, testFile name) of
            L.FORMAT_NOT_SUPPORTED => [setupFailedTest "Format not supported"]
          | L.SYSTEM_ERROR err => [setupFailedTest err]
          | L.PARSE_ERROR err => [setupFailedTest err]
          | L.OK store =>
            let val n1 = length (S.enumerate store)
		val tt = testsFromStore store
		val n2 = length tt
	    in
                if n1 < 100 
                then [setupFailedTest
                          ("too few (" ^ (Int.toString n1) ^
                           ") triples in manifest (load problem?)")]
		else if n2 < 20 
                then [setupFailedTest
                          ("too few (" ^ (Int.toString n2) ^
                           ") tests found in spec store (load problem?)")]
                else tt
            end
                
    fun tests () = testsFromManifest "manifest.ttl"
                     
end

structure TestTurtleSpec = TestTurtleSpecFn(TurtleParser)

                                           
