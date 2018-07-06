
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

    structure TNC = TurtleNTriplesConverter
                      
    fun convertDirectToNtriples (inBase, infile)
                                (outBase, outfile) =
        let fun convert' () =
            let
                val instream = CodepointIO.openIn infile
                val outstream = CodepointIO.openOut outfile
                val result = TNC.convert (inBase, instream)
                                         (outBase, outstream)
            in
                CodepointIO.closeOut outstream;
                CodepointIO.closeIn instream;
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
            
    fun convertViaStore (inBase, inFile) (outBase, outFile) =
        case Loader.loadFileAsNewStore (inBase, inFile) of
            Loader.FORMAT_NOT_SUPPORTED => INPUT_FORMAT_NOT_SUPPORTED
          | Loader.SYSTEM_ERROR err => SYSTEM_ERROR err
          | Loader.PARSE_ERROR err => CONVERSION_ERROR err
          | Loader.OK (base, store) =>
            case Exporter.saveToFile store
                                     (case outBase of
                                          NONE => base 
                                        | _ => outBase,
                                      outFile) of
                Exporter.FORMAT_NOT_SUPPORTED => OUTPUT_FORMAT_NOT_SUPPORTED
              | Exporter.SYSTEM_ERROR err => SYSTEM_ERROR err
              | _ => OK
                         
    fun convert (inBase, inFile) (outBase, outFile) = 
        (if (FileType.formatOf inFile = FileType.TURTLE orelse
             FileType.formatOf inFile = FileType.NTRIPLES) andalso
            FileType.formatOf outFile = FileType.NTRIPLES
         then convertDirectToNtriples
         else convertViaStore) (inBase, inFile)
                               (outBase, outFile)

    (*!!! todo: convert to stream with a given format specified in a
    string separately. Could possibly adapt the file converter / file
    exporter etc to accept that as an alternative as well. And for
    input? *)
                
end


                        
