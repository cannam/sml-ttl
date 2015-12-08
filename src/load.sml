
open RdfTriple
	 
fun usage () =
    let open TextIO in
	output (stdErr,
	    "Usage:\n" ^
            "    load filename.ttl\n");
        raise Fail "Incorrect arguments specified"
    end

fun load_file filename =
    let open TurtleParser
    in
        case parse_file "file:///blah" filename of
            PARSE_ERROR e => raise Fail e
          | PARSED p =>
            (*            app (fn t => print ((string_of_triple t) ^ "\n")) (#triples p) *)
            print ("Loaded " ^ (Int.toString (length (#triples p))) ^ " triple(s)\n")
    end
        
fun main () =
    (case CommandLine.arguments () of
        [filename] => load_file filename (*!!! + base iri *)
      | _ => usage ())
    handle Fail msg => TextIO.output (TextIO.stdErr, "Exception: " ^ msg ^ "\n")

