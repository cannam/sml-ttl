
(*!!! error handling -- make consistent between these and store-io.

    Currently we raise exceptions for (i) unsupported format extension
    and (ii) system file access problems (raised by
    e.g. TextIO.openIn), but return an error type for (iii) parse
    errors.
*)

structure RdfFileParser :> RDF_FILE_PARSER = struct

    open RdfParserBase
                        
    fun parse_file base_iri filename =
        let open FileType

            val parser =
                case format_of filename of
                    TURTLE => TurtleParser.parse
                  | NTRIPLES => TurtleParser.parse
                  | _ => raise Fail "Unknown or unsupported file extension"

            val stream = TextIO.openIn filename
            val result = parser base_iri stream
        in
            TextIO.closeIn stream;
            result
        end

end
