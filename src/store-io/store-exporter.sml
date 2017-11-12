
functor StoreIncrementalExporterFn (S: RDF_INCREMENTAL_SERIALISER)
        :> STORE_EXPORTER
               where type store = Store.t
= struct

    type store = Store.t

    fun save_to_stream store stream =
        let val serialiser = S.new stream
        in
            S.finish (S.serialise (serialiser, Store.enumerate store))
        end
            
    fun save_to_file store filename =
        let val stream = TextIO.openOut filename
            val _ = save_to_stream store stream
        in
            TextIO.closeOut stream
        end
end

functor StoreAbbreviatingExporterFn
            (S: RDF_ABBREVIATING_SERIALISER
                    where type prefix_table = PrefixTable.t
                    where type matcher = Store.t)
        :> STORE_EXPORTER
               where type store = Store.t
= struct

    type store = Store.t
                     
    fun save_to_stream store stream =
        let val serialiser = S.new (Store.get_prefix_table store, store) stream
        in
            S.finish (S.serialise (serialiser, Store.enumerate store))
        end

    fun save_to_file store filename =
        let val stream = TextIO.openOut filename
            val _ = save_to_stream store stream
        in
            TextIO.closeOut stream
        end
end
					    
structure NTriplesExporter =
    StoreIncrementalExporterFn(NTriplesSerialiser)

structure TurtleExporter =
    StoreAbbreviatingExporterFn(TurtleSerialiserFn(struct
                                                    structure P = PrefixTable
                                                    structure M = Store
                                                    end))

structure StoreStreamExporter
          :> STORE_STREAM_EXPORTER where type store = Store.t = struct

    type store = Store.t

    fun save_to_stream store (format, stream) =
        let open FileType
            val exporter = 
                case format of
                    TURTLE => TurtleExporter.save_to_stream
                  | NTRIPLES => NTriplesExporter.save_to_stream
                  | _ => raise Fail "Unknown or unsupported save format"
        in
            exporter store stream
        end

    val formats_supported = [FileType.TURTLE, FileType.NTRIPLES]
            
end

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

