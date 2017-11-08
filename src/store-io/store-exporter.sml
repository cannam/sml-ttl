
functor StoreIncrementalExporterFn (S: RDF_INCREMENTAL_SERIALISER)
        :> STORE_EXPORTER where type store = Store.t = struct

    type store = Store.t

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
					    
structure NTriplesExporter = StoreIncrementalExporterFn(NTriplesSerialiser)

structure StoreFileExporter
          :> STORE_FILE_EXPORTER where type store = Store.t = struct

    type store = Store.t

    fun save_to_file store filename =
        let open FileType
            val exporter = 
                case type_of filename of
                    NTRIPLES => NTriplesExporter.save_to_file
                  | TURTLE => TurtleExporter.save_to_file
                  | _ => raise Fail "Unknown or unsupported file extension"
        in
            exporter store filename
        end
                                  
end

