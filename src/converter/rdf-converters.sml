
structure TurtleNTriplesConverter =
    RdfIncrementalConverterFn(struct
                               structure Parser = TurtleIncrementalParser
                               structure Serialiser = NTriplesSerialiser
                               end)

structure FileExtensionDrivenConverter :> RDF_FILE_CONVERTER = struct

    type prefix = RdfTriple.prefix
    type triple = RdfTriple.triple

    type base_iri = string

    open RdfConverterBase

    (*!!! what to do with file exceptions, e.g. the Fail thrown by
    StoreFileLoader.load_file_as_new_store for unknown file extension,
    or a failure to open a file at all? *)

    (*!!! need end-to-end tests for file conversions *)
                          
    (*!!! the parsers/serialisers should declare their expected file extensions *)
    fun is_incremental infile outfile =
        case FileType.format_of outfile of
            FileType.NTRIPLES => true
          | _ => false
                              
    fun convert_incremental base_iri infile outfile =
        let
            val instream = TextIO.openIn infile
            val outstream = TextIO.openOut outfile
            val result = 
                case TurtleNTriplesConverter.convert base_iri
                                                     instream outstream of
                    CONVERSION_ERROR e => CONVERSION_ERROR e
                  | CONVERTED => CONVERTED
        in
            TextIO.closeOut outstream;
            TextIO.closeIn instream;
            result
        end
                              
    fun convert_via_store base_iri infile outfile =
        case StoreFileLoader.load_file_as_new_store base_iri infile of
            StoreFileLoader.LOAD_ERROR err => CONVERSION_ERROR err
          | StoreFileLoader.OK store =>
            (StoreFileExporter.save_to_file store outfile;
             CONVERTED)

    fun convert base_iri infile outfile =
        if is_incremental infile outfile then
            convert_incremental base_iri infile outfile
        else
            convert_via_store base_iri infile outfile

    (*!!! todo: convert to stream with a given format specified in a
    string separately. Could possibly adapt the file converter / file
    exporter etc to accept that as an alternative as well. And for
    input? *)
                
end


                        
