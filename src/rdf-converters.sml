
structure TurtleNTriplesConverter =
    RdfConverterStreamFn(struct
                          structure Parser = TurtleStreamParser
                          structure Serialiser = NTriplesSerialiser
                          end)

structure FileExtensionDrivenConverter : RDF_FILE_CONVERTER = struct

    (* goes through a Store *)

    type prefix = RdfTriple.prefix
    type triple = RdfTriple.triple

    type base_iri = string

    datatype result = CONVERSION_ERROR of string |
                      CONVERTED

    (*!!! what to do with file exceptions, e.g. the Fail thrown by
    StoreFileLoader.load_file_as_new_store for unknown file extension,
    or a failure to open a file at all? *)
                          
    fun convert base_iri infile outfile =
        case StoreFileLoader.load_file_as_new_store base_iri infile of
            StoreFileLoader.LOAD_ERROR err => CONVERSION_ERROR err
          | StoreFileLoader.OK store =>
            (StoreFileExporter.save_to_file store outfile;
             CONVERTED)

    (*!!! todo: convert to stream with a given format specified in a
    string separately. Could possibly adapt the file converter / file
    exporter etc to accept that as an alternative as well. And for
    input? *)
                
end


                        
