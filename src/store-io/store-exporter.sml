
functor StoreIncrementalExporterFn (S: RDF_INCREMENTAL_SERIALISER)
        :> STORE_EXPORTER where type store = Store.t = struct

    type store = Store.t

    fun save_to_stream store stream =
        let val serialiser = S.new stream
        in
            S.serialise (serialiser, Store.enumerate store);
            S.finish serialiser
        end
            
    fun save_to_file store filename =
        let val stream = TextIO.openOut filename
            val _ = save_to_stream store stream
        in
            TextIO.closeOut stream
        end
			  
end
					    
structure NTriplesExporter = StoreIncrementalExporterFn(NTriplesSerialiser)

structure StoreFileExporter
          :> STORE_FILE_EXPORTER where type store = Store.t = struct

    type store = Store.t

    fun save_to_file store filename =
        let open FileType
            val exporter = 
                case format_of filename of
                    TURTLE => TurtleExporter.save_to_file
                  | NTRIPLES => NTriplesExporter.save_to_file
                  | _ => raise Fail "Unknown or unsupported file extension"
        in
            exporter store filename
        end

    val formats_supported = [FileType.TURTLE, FileType.NTRIPLES]
            
    val extensions_supported = 
        List.concat (map FileType.extensions_for_format formats_supported)
            
end

