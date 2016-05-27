
(* This assumes src/sml-ttl.mlb *)

(* 1. Read all the triples from an RDF/Turtle file *)

fun read_triples_example () =
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

(*!!! can't yet do this! need RDF_FILE_PARSER *)
        
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
            LOAD_ERROR text => (print ("Load failed: " ^ text ^ "\n"); Store.empty)
          | OK store => store
    end

(* 5. Load a Turtle file into a store, query it, and save the results
      as an NTriples file *)


fun main () =
    let
        val triples_from_turtle = read_triples_example ()
        val store_from_turtle = load_to_store_example ()
    in

        print ("Parsed " ^
               (Int.toString (length triples_from_turtle)) ^
               " triple(s) from Turtle file\n");

        print ("Loaded " ^
               (Int.toString (length (Store.enumerate store_from_turtle))) ^
               " triple(s) into store from Turtle file\n")
    end
        
        
