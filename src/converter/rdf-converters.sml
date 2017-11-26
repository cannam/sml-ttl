
structure TurtleNTriplesConverter =
    RdfIncrementalConverterFn(struct
                               structure Parser = TurtleIncrementalParser
                               structure Serialiser = NTriplesSerialiser
                               end)

structure FileExtensionDrivenConverter :> RDF_FILE_CONVERTER = struct

    type prefix = Prefix.prefix
    type triple = RdfTriple.triple
    type base_iri = BaseIri.t

    datatype result =
             INPUT_FORMAT_NOT_SUPPORTED |
             OUTPUT_FORMAT_NOT_SUPPORTED |
             SYSTEM_ERROR of string |
             CONVERSION_ERROR of string |
             OK

    (*!!! need end-to-end tests for file conversions *)
                            
    structure TNC = TurtleNTriplesConverter
                      
    fun convert_direct_to_ntriples (in_base, infile)
                                   (out_base, outfile) =
        let fun convert' () =
            let
                val instream = TextIO.openIn infile
                val outstream = TextIO.openOut outfile
                val result = TNC.convert (in_base, instream)
                                         (out_base, outstream)
            in
                TextIO.closeOut outstream;
                TextIO.closeIn instream;
                case result of
                    TNC.CONVERSION_ERROR err => CONVERSION_ERROR err
                  | TNC.OK => OK
            end
        in
            convert' ()
            handle ex => SYSTEM_ERROR (exnMessage ex)
        end

    structure Loader = StoreFileLoader
    structure Exporter = StoreFileExporter
            
    fun convert_via_store (in_base, infile)
                          (out_base, outfile) =
        case Loader.load_file_as_new_store (in_base, infile) of
            Loader.FORMAT_NOT_SUPPORTED => INPUT_FORMAT_NOT_SUPPORTED
          | Loader.SYSTEM_ERROR err => SYSTEM_ERROR err
          | Loader.PARSE_ERROR err => CONVERSION_ERROR err
          | Loader.OK store =>
            case Exporter.save_to_file store (out_base, outfile) of
                Exporter.FORMAT_NOT_SUPPORTED => OUTPUT_FORMAT_NOT_SUPPORTED
              | Exporter.SYSTEM_ERROR err => SYSTEM_ERROR err
              | _ => OK
                         
    fun convert (in_base, infile) (out_base, outfile) = 
        (if (FileType.format_of infile = FileType.TURTLE orelse
             FileType.format_of infile = FileType.NTRIPLES) andalso
            FileType.format_of outfile = FileType.NTRIPLES
         then convert_direct_to_ntriples
         else convert_via_store) (in_base, infile)
                                 (out_base, outfile)

    (*!!! todo: convert to stream with a given format specified in a
    string separately. Could possibly adapt the file converter / file
    exporter etc to accept that as an alternative as well. And for
    input? *)
                
end


                        
