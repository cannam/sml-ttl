
open RdfTriple
	 
fun usage () =
    let open TextIO in
	output (stdErr,
	    "Usage:\n" ^
            "    convert infile [outfile]\n");
        raise Fail "Incorrect arguments specified"
    end

fun report_time text start =
    TextIO.output (TextIO.stdErr, 
                   text ^ ": " ^
                   (Real.toString (Time.toReal
                                       (Time.- (Time.now (), start)))) ^ " sec\n")

fun convert_stdout iri infile =
    let val start = Time.now ()
        val instream = TextIO.openIn infile
        val outstream = TextIO.stdOut
        open TurtleNTriplesConverter
        val result = convert iri instream outstream
    in
        TextIO.closeIn instream;
        case result of
            CONVERSION_ERROR err => raise Fail err
          | CONVERTED => ()
    end

fun convert_file iri (infile, outfile) =
    let val start = Time.now ()
        val instream = TextIO.openIn infile
        val outstream = TextIO.openOut outfile
        open FileExtensionDrivenConverter
        val result = convert iri infile outfile
    in
        TextIO.closeIn instream;
        TextIO.closeOut outstream;
        case result of
            CONVERSION_ERROR err => raise Fail err
          | CONVERTED => ()
    end
        
fun handle_args args =
    case args of
        "-v"::rest => (Log.setLogLevel Log.INFO ; handle_args rest)
      | [infile, outfile] => convert_file "blah" (infile, outfile) (*!!! + base iri *)
      | [infile] => convert_stdout "blah" infile (*!!! + base iri *)
      | _ => usage ()
           
fun main () =
    handle_args (CommandLine.arguments ())
    handle Fail msg => TextIO.output (TextIO.stdErr, "Exception: " ^ msg ^ "\n")

