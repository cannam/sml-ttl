
(* This assumes src/sml-ttl.mlb *)

(* 1. Read all the triples from an RDF/Turtle file *)

fun read_turtle_stream_example () =
    let
        val filename = "test/other/goblin.ttl"
        val base_iri = "file:///" ^ filename
        val stream = TextIO.openIn filename
        open TurtleParser
    in
        case parse base_iri stream of
            PARSE_ERROR text => (print ("Parse failed: " ^ text ^ "\n"); [])
          | PARSED { prefixes, triples } => triples
    end

(* 2. Read all the triples from an RDF document of some sort (without
      having to specify Turtle) *)
(*
fun read_any_file_example () =
    let
        val filename = "test/other/goblin.ttl"
        val base_iri = "file:///" ^ filename
        open RdfFileParser
    in
        case parse_file base_iri filename of
            PARSE_ERROR text => (print ("Parse failed: " ^ text ^ "\n"); [])
          | PARSED { prefixes, triples } => triples
    end
*)
(* 3. Read all the triples from a remote URL serving an RDF document *)

(* !!! *)

(* 4. Load a Turtle file into a store and query it *)

fun load_to_store_example () =
    let
        val filename = "test/other/goblin.ttl"
        val base_iri = "file:///" ^ filename
        open StoreFileLoader                                        
    in
        case load_file_as_new_store base_iri filename of
            FORMAT_NOT_SUPPORTED =>
            (print "Format not supported!\n"; Store.empty)
          | SYSTEM_ERROR err =>
            (print ("System error: " ^ err ^ "\n"); Store.empty)
          | PARSE_ERROR err =>
            (print ("Load failed: " ^ err ^ "\n"); Store.empty)
          | OK store => store
    end

(* 5. Load a Turtle file into a store, query it, and save the results
      as an NTriples file *)
(*!!! todo: query it *)
fun conversion_example_1 () =
    let
        val filename = "test/other/goblin.ttl"
        val outfile = "test/out/temporary1.ntriples"
        val base_iri = "file:///" ^ filename
        val loaded =
            let open StoreFileLoader                                        
            in
                case load_file_as_new_store base_iri filename of
                    FORMAT_NOT_SUPPORTED =>
                    (print "Format not supported!\n"; NONE)
                  | SYSTEM_ERROR err =>
                    (print ("System error: " ^ err ^ "\n"); NONE)
                  | PARSE_ERROR err =>
                    (print ("Load failed: " ^ err ^ "\n"); NONE)
                  | OK store => SOME store
            end
    in
        case loaded of
            NONE => false
          | SOME store => 
            (StoreFileExporter.save_to_file store outfile =
             StoreFileExporter.OK)
    end

(* 6. Do that again but using an RDF converter *)

fun conversion_example_2 () =
    let
        val filename = "test/other/goblin.ttl"
        val outfile = "test/out/temporary2.ntriples"
        val base_iri = "file:///" ^ filename
        open FileExtensionDrivenConverter
    in
        case convert (base_iri, filename) (base_iri, outfile) of
            INPUT_FORMAT_NOT_SUPPORTED =>
            (print "Input format not supported!\n"; false)
          | OUTPUT_FORMAT_NOT_SUPPORTED =>
            (print "Input format not supported!\n"; false)
          | SYSTEM_ERROR err =>
            (print ("System failed: " ^ err ^ "\n"); false)
          | CONVERSION_ERROR err =>
            (print ("Conversion failed: " ^ err ^ "\n"); false)
          | CONVERTED => true
    end


        
fun main () =
    let
        val triples_from_turtle = read_turtle_stream_example ()
(*!!!        val triples_from_any = read_any_file_example () *)
        val store_from_turtle = load_to_store_example ()
        val converted_1 = conversion_example_1 ()
        val converted_2 = conversion_example_2 ()
    in

        print ("Parsed " ^
               (Int.toString (length triples_from_turtle)) ^
               " triple(s) from Turtle file\n");
(*!!!
        print ("Parsed " ^
               (Int.toString (length triples_from_any)) ^
               " triple(s) from file by extension\n");
*)
        print ("Loaded " ^
               (Int.toString (length (Store.enumerate store_from_turtle))) ^
               " triple(s) into store from file\n");

        print ("Conversion example 1 result: " ^
               (Bool.toString converted_1) ^ "\n");

        print ("Conversion example 2 result: " ^
               (Bool.toString converted_2) ^ "\n")
              
    end
        
        
