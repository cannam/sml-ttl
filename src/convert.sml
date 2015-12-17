
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
        
fun convert iri filename =
    (* !!! should be a functor *)
    let val start = Time.now ()
        fun serialise serialiser prefixes triples =
            foldl (fn (t, s) =>
                      NTriplesSerialiser.serialise
                          (s, NTriplesSerialiser.TRIPLE t))
                  (foldl (fn (p, s) => NTriplesSerialiser.serialise
                                           (s, NTriplesSerialiser.PREFIX p))
                         serialiser
                         prefixes)
                  triples
        fun parse' acc f =
            case f () of
                TurtleStreamParser.END_OF_STREAM => ()
              | TurtleStreamParser.PARSE_ERROR err => raise Fail err
              | TurtleStreamParser.PARSE_OUTPUT ({ prefixes, triples }, f') =>
                parse' (serialise acc prefixes triples) f'
        val instream = TextIO.openIn filename
        val outstream = TextIO.stdOut
    in
        parse' (NTriplesSerialiser.new outstream)
               (fn () => TurtleStreamParser.parse iri instream);
        TextIO.closeIn instream 
    end
        
fun main () =
    (case CommandLine.arguments () of
        [filename] => convert "blah" filename (*!!! + base iri *)
      | _ => usage ())
    handle Fail msg => TextIO.output (TextIO.stdErr, "Exception: " ^ msg ^ "\n")

