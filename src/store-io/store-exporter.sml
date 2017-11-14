
structure StoreExportBase : STORE_EXPORT_BASE = struct

    type base_iri = string

    datatype result =
             FORMAT_NOT_SUPPORTED |
             SYSTEM_ERROR of string |
             OK

    type store = Store.t
                 
end

functor StoreIncrementalExporterFn (S: RDF_INCREMENTAL_SERIALISER)
        :> STORE_EXPORTER
               where type store = Store.t
               where type result = StoreExportBase.result
= struct

    open StoreExportBase

    fun save_to_stream store _ stream =
        let val serialiser = S.new stream
        in
            S.finish (S.serialise (serialiser, Store.enumerate store));
            OK
        end
            
    fun save_to_file' store filename =
        let val stream = TextIO.openOut filename
            val _ = save_to_stream store stream
        in
            TextIO.closeOut stream;
            OK
        end

    fun save_to_file store _ filename =
        save_to_file' store filename
        handle ex => SYSTEM_ERROR (exnMessage ex)
end

functor StoreAbbreviatingExporterFn
            (S: RDF_ABBREVIATING_SERIALISER
                    where type prefix_table = PrefixTable.t
                    where type matcher = Store.t)
        :> STORE_EXPORTER
               where type store = Store.t
               where type result = StoreExportBase.result
= struct

    open StoreExportBase
                     
    fun save_to_stream store base_iri stream =
        let val serialiser =
                S.new (base_iri, Store.get_prefix_table store, store) stream
        in
            S.finish (S.serialise (serialiser, Store.enumerate store));
            OK
        end

    fun save_to_file' store base_iri filename =
        let val stream = TextIO.openOut filename
            val _ = save_to_stream store base_iri stream
        in
            TextIO.closeOut stream;
            OK
        end

    fun save_to_file store base_iri filename =
        save_to_file' store base_iri filename
        handle ex => SYSTEM_ERROR (exnMessage ex)
end
					    
structure NTriplesExporter =
    StoreIncrementalExporterFn(NTriplesSerialiser)

structure TurtleExporter =
    StoreAbbreviatingExporterFn(TurtleSerialiserFn(struct
                                                    structure P = PrefixTable
                                                    structure M = Store
                                                    end))

structure StoreStreamExporter
          :> STORE_STREAM_EXPORTER
               where type store = Store.t
               where type result = StoreExportBase.result
= struct

    open StoreExportBase

    exception Unsupported

    fun save_to_stream store base_iri (format, stream) =
        let open FileType
            val exporter = 
                case format of
                    TURTLE => TurtleExporter.save_to_stream
                  | NTRIPLES => NTriplesExporter.save_to_stream
                  | _ => raise Unsupported
        in
            (exporter store base_iri stream; OK)
            handle Unsupported => FORMAT_NOT_SUPPORTED
            handle ex => SYSTEM_ERROR (exnMessage ex)
        end

    val formats_supported = [FileType.TURTLE, FileType.NTRIPLES]
            
end

structure StoreFileExporter
          :> STORE_FILE_EXPORTER
               where type store = Store.t
               where type result = StoreExportBase.result
= struct

    open StoreExportBase

    exception Unsupported

    fun save_to_file store base_iri filename =
        let open FileType
            val exporter = 
                case format_of filename of
                    TURTLE => TurtleExporter.save_to_file
                  | NTRIPLES => NTriplesExporter.save_to_file
                  | _ => raise Unsupported
        in
            (exporter store base_iri filename; OK)
            handle Unsupported => FORMAT_NOT_SUPPORTED
            handle ex => SYSTEM_ERROR (exnMessage ex)
        end

    val formats_supported = [FileType.TURTLE, FileType.NTRIPLES]
            
    val extensions_supported = 
        List.concat (map FileType.extensions_for_format formats_supported)
            
end

