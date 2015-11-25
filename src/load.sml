
fun string_of_node (IRI iri) = "<" ^ iri ^ ">"
  | string_of_node (BLANK n) = "_" ^ (Int.toString n)
  | string_of_node (LITERAL lit) = "\"" ^ (#value lit) ^ "\"" ^
                                   (if #dtype lit = "" then ""
                                    else "^^" ^ (#dtype lit)) ^
                                   (if #lang lit = "" then ""
                                    else "@" ^ (#lang lit))

fun string_of_triple (a,b,c) =
    "(" ^ (string_of_node a) ^
    "," ^ (string_of_node b) ^
    "," ^ (string_of_node c) ^
    ")"

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

