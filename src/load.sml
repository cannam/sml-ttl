
open RdfTriple
	 
fun usage () =
    let open TextIO in
	output (stdErr,
	    "Usage:\n" ^
            "    load filename.ttl\n");
        raise Fail "Incorrect arguments specified"
    end

fun report_time text start =
    TextIO.output (TextIO.stdErr, 
                   text ^ ": " ^
                   (Real.toString (Time.toReal
                                       (Time.- (Time.now (), start)))) ^ " sec\n")
        
fun load_file filename =
    let val start = Time.now () in 
        (*        case TurtleLoader.load_file_as_new_store "some_iri" filename of *)
        case StoreFileLoader.load_file_as_new_store "some_iri" filename of
(*            TurtleLoader.LOAD_ERROR e => raise Fail e
          | TurtleLoader.OK store => *)
            StoreFileLoader.LOAD_ERROR e => raise Fail e
          | StoreFileLoader.OK store =>
            (report_time "Load complete" start;
             print ("Loaded " ^ (Int.toString (List.length (Store.enumerate store))) ^ " triple(s):\n");
             NTriplesSaver.save_to_stream store TextIO.stdOut)
(*            (app (fn t => print ((string_of_triple t) ^ "\n")) (#triples p) ;
             print ("Loaded " ^ (Int.toString (length (#triples p))) ^ " triple(s)\n"))
        *)
    end
        
fun main () =
    (case CommandLine.arguments () of
        [filename] => load_file filename (*!!! + base iri *)
      | _ => usage ())
    handle Fail msg => TextIO.output (TextIO.stdErr, "Exception: " ^ msg ^ "\n")

