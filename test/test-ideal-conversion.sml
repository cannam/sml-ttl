
structure TestIdealConversion : TESTS = struct

    open TestSupport

    val name = "ideal-conversion"

    val testFileDir = "test/other"
    val referenceDir = "test/other/ideal"
    val outFileDir = "test/out"

    fun convertFile (infile, outfile) =
        let val instream = TextIO.openIn infile
            val outstream = TextIO.openOut outfile
            open FileExtensionDrivenConverter
            val result = convert (NONE, infile) (NONE, outfile)
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

    fun linesOf f =
        let fun lines' s =
                case TextIO.inputLine s of
                    SOME l => l :: lines' s
                  | NONE => []
            val s = TextIO.openIn f
            val lines = lines' s
        in
            TextIO.closeIn s;
            lines
        end

    fun checkConversion infile outfile reference =
        let val _ = convertFile (infile, outfile)
            val obtained = linesOf outfile
            val expected = linesOf reference
        in
            if obtained = expected
            then true
            else
                (print ("\n--- Conversion did not produce ideal output: " ^
                        "compare against reference with\n      " ^
                        "diff -u " ^ outfile ^ " " ^ reference ^ "\n");
                 false)
        end
      
    fun tests () =
        map (fn f =>
                (f, fn () => checkConversion
                                 (testFileDir ^ "/" ^ f ^ ".ttl")
                                 (outFileDir ^ "/" ^ f ^ ".ttl")
                                 (referenceDir ^ "/" ^ f ^ ".ttl")))
            [ "bnode-nested-2", "bnode-nested", "bnode", "boolean",
              "collections", "example1", "example2", "example3", "goblin",
              "iris", "numbers", "quoted", "quoted2" ]
      
end
