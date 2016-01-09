
open RdfTriple
	 
fun usage () =
    let open TextIO in
	output (stdErr,
	    "Usage:\n" ^
            "    convert filename.ttl\n");
        raise Fail "Incorrect arguments specified"
    end

fun report_time text start =
    TextIO.output (TextIO.stdErr, 
                   text ^ ": " ^
                   (Real.toString (Time.toReal
                                       (Time.- (Time.now (), start)))) ^ " sec\n")

fun convert iri filename =
    let val start = Time.now ()
        val instream = TextIO.openIn filename
        val outstream = TextIO.stdOut
        open TurtleNTriplesConverter
        val result = convert iri instream outstream
    in
        TextIO.closeIn instream;
        case result of
            CONVERSION_ERROR err => raise Fail err
          | CONVERTED => ()
    end
        
fun main () =
    (case CommandLine.arguments () of
        [filename] => convert "blah" filename (*!!! + base iri *)
      | _ => usage ())
    handle Fail msg => TextIO.output (TextIO.stdErr, "Exception: " ^ msg ^ "\n")

