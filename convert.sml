
open RdfTriple
         
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
            "If outfile is absent, output will go to stdout " ^
            "and will be in NTriples format.\n" ^
            "\nSupported formats are:\n" ^
            "  import: " ^ formatDesc (StoreFileLoader.formats_supported) ^
            "\n  output: " ^ formatDesc (StoreFileExporter.formats_supported) ^
            "\n\n");
        raise Fail "Incorrect arguments specified"
    end

fun convert_stdout iri infile =
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

fun convert_file iri (infile, outfile) =
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
        
fun handle_args args =
    case args of
        "-v"::rest => (Log.setLogLevel Log.INFO ; handle_args rest)
      | [infile, outfile] =>
        convert_file NONE (infile, outfile) (*!!! + base iri *)
      | [infile] =>
        convert_stdout NONE infile (*!!! + base iri *)
      | _ => usage ()
           
fun main () =
    handle_args (CommandLine.arguments ())
    handle Fail msg => TextIO.output (TextIO.stdErr, "Exception: " ^ msg ^ "\n")

