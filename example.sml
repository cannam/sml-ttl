
(* This assumes src/sml-ttl.mlb *)

(* 1. Read all the triples from an RDF/Turtle file, using the Turtle
   parser directly, and do something with them (print them out, in
   this case) *)
fun read_turtle_stream_example () =
    let
        val filename = "test/other/goblin.ttl"
        val base_iri = SOME (Iri.fromString ("file:///" ^ filename))
        val stream = TextIO.openIn filename
        open TurtleParser
    in
        case parse (base_iri, stream) of
            PARSE_ERROR text =>
            print ("Parse failed: " ^ text ^ "\n")
          | PARSED { triples, ... } =>
	    print ("Parse succeeded, have " ^
                   (Int.toString (length triples)) ^
		   " triple(s) as follows:\n" ^
		   (RdfTriple.string_of_triples triples) ^ "\n")
    end

(* 2. Load an RDF file of any supported format into a store and
   extract all the triples from it *)
fun load_to_store_example () =
    let
        val filename = "test/other/goblin.ttl"
        open StoreFileLoader                                        
    in
        case load_file_as_new_store (NONE, filename) of
            FORMAT_NOT_SUPPORTED =>
            print "Format not supported!\n"
          | SYSTEM_ERROR err =>
            print ("System error: " ^ err ^ "\n")
          | PARSE_ERROR err =>
            print ("Load failed: " ^ err ^ "\n")
          | OK store =>
            let val triples = Store.enumerate store
            in
                print ("Load succeeded, have " ^
                       (Int.toString (length triples)) ^
		   " triple(s) as follows:\n" ^
		   (RdfTriple.string_of_triples triples) ^ "\n")
            end
    end

(* 3. Convert between two RDF file formats *)
fun conversion_example () =
    let
        val infile = "test/other/goblin.ttl"
        val outfile = "test/out/temporary2.ntriples"
        open FileExtensionDrivenConverter
    in
        case convert (NONE, infile) (NONE, outfile) of
            INPUT_FORMAT_NOT_SUPPORTED =>
            print "Input format not supported!\n"
          | OUTPUT_FORMAT_NOT_SUPPORTED =>
            print "Input format not supported!\n"
          | SYSTEM_ERROR err =>
            print ("System failed: " ^ err ^ "\n")
          | CONVERSION_ERROR err =>
            print ("Conversion failed: " ^ err ^ "\n")
          | CONVERTED =>
            print "Conversion succeeded\n"
    end

fun main () =
    let
        val _ = read_turtle_stream_example ()
        val _ = load_to_store_example ()
        val _ = conversion_example ()
    in
        ()              
    end
        
        
