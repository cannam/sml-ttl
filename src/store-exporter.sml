
functor StoreStreamExporterFn (S: RDF_STREAM_SERIALISER) : STORE_EXPORTER = struct

    structure Store = Store

    fun save_to_stream store stream =
        let val serialiser = S.new stream
        in
            Store.foldl (fn (t, s) => S.serialise (s, S.TRIPLE t))
                        (List.foldl (fn (p, s) => S.serialise (s, S.PREFIX p))
                                    serialiser
                                    (Store.enumerate_prefixes store))
                        store;
            ()
        end
            
    fun save_to_file store filename =
        let val stream = TextIO.openOut filename
            val _ = save_to_stream store stream
        in
            TextIO.closeOut stream
        end
			  
end
					    
structure NTriplesExporter = StoreStreamExporterFn(NTriplesSerialiser)

structure StoreFileExporter : STORE_FILE_EXPORTER = struct

    structure Store = Store

    fun save_to_file store filename =
        let val exporter = 
                case FileExtension.extension filename of
                    "ntriples" => NTriplesExporter.save_to_file
                  | "nt" => NTriplesExporter.save_to_file
                  | "ttl" => TurtleExporter.save_to_file
                  | other => raise Fail ("Unknown or unsupported file extension \""
                                         ^ other ^ "\"")
        in
            exporter store filename
        end
                                  
end

