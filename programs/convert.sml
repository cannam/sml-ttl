
open RdfTriple

(*!!! todo: make clear in usage how output format is selected; support turtle output to stdout (it works to file only at the mo) *)
         
fun usage () =
    let open TextIO
        fun formatDesc formats =
            String.concatWith
                ", "
                (map (fn f =>
                         FileType.name_for_format f ^ " (" ^
                         String.concatWith
                             ", "
                             (map (fn e => "*." ^ e)
                                  (FileType.extensions_for_format f))
                         ^ ")")
                     formats)
    in
	output (stdErr,
	    "\nUsage:\n" ^
            "    convert infile [outfile]\n" ^
            "\nFile format is deduced from the file extension.\n" ^
            "If outfile is absent, output will go to stdout and will be in NTriples format.\n" ^
            "\nSupported formats are:\n" ^
            "For import: " ^ formatDesc (StoreFileLoader.formats_supported) ^
            "\nFor output: " ^ formatDesc (StoreFileExporter.formats_supported) ^
            "\n\n");
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

