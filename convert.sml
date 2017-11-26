
open RdfTriple
         
fun usage () =
    let open TextIO
        fun formatDesc formats =
            String.concatWith
                ", "
                (map (fn f =>
                         FileType.nameForFormat f ^ " (" ^
                         String.concatWith
                             ", "
                             (map (fn e => "*." ^ e)
                                  (FileType.extensionsForFormat f))
                         ^ ")")
                     formats)
    in
	output (stdErr,
	    "\nUsage:\n" ^
            "    convert infile [outfile]\n" ^
            "\nFile format is deduced from the file extension.\n" ^
            "If outfile is absent, output will go to stdout " ^
            "and will be in NTriples format.\n" ^
            "\nSupported formats are:\n" ^
            "  import: " ^ formatDesc (StoreFileLoader.formatsSupported) ^
            "\n  output: " ^ formatDesc (StoreFileExporter.formatsSupported) ^
            "\n\n");
        raise Fail "Incorrect arguments specified"
    end

fun convertStdout iri infile =
    let val instream = TextIO.openIn infile
        val outstream = TextIO.stdOut
        open TurtleNTriplesConverter
        val result = convert (iri, instream) (iri, outstream)
    in
        TextIO.closeIn instream;
        case result of
            CONVERSION_ERROR err => raise Fail err
          | CONVERTED => ()
    end

fun convertFile iri (infile, outfile) =
    let val instream = TextIO.openIn infile
        val outstream = TextIO.openOut outfile
        open FileExtensionDrivenConverter
        val result = convert (iri, infile) (iri, outfile)
    in
        TextIO.closeIn instream;
        TextIO.closeOut outstream;
        case result of
            INPUT_FORMAT_NOT_SUPPORTED =>
            raise Fail "Input format not supported"
          | OUTPUT_FORMAT_NOT_SUPPORTED =>
            raise Fail "Output format not supported"
          | SYSTEM_ERROR err => raise Fail ("System error: " ^ err)
          | CONVERSION_ERROR err => raise Fail err
          | CONVERTED => ()
    end
        
fun handleArgs args =
    case args of
        "-v"::rest => (Log.setLogLevel Log.INFO ; handleArgs rest)
      | [infile, outfile] =>
        convertFile NONE (infile, outfile) (*!!! + base iri *)
      | [infile] =>
        convertStdout NONE infile (*!!! + base iri *)
      | _ => usage ()
           
fun main () =
    handleArgs (CommandLine.arguments ())
    handle Fail msg => TextIO.output (TextIO.stdErr, "Exception: " ^ msg ^ "\n")

