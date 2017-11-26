
structure StoreExportBase : STORE_EXPORT_BASE = struct

    type baseIri = BaseIri.t
    type store = Store.t
                 
    datatype result =
             FORMAT_NOT_SUPPORTED |
             SYSTEM_ERROR of string |
             OK

end

functor StoreIncrementalExporterFn (S: RDF_INCREMENTAL_SERIALISER)
        :> STORE_EXPORTER
               where type store = Store.t
               where type result = StoreExportBase.result
= struct

    open StoreExportBase

    fun saveToStream store (baseIri, stream) =
        (* incremental serialiser doesn't use outBase iri *)
        let val serialiser = S.new stream
        in
            S.finish (S.serialise (serialiser, Store.enumerate store));
            OK
        end
            
    fun saveToFile' store (baseIri, filename) =
        let val stream = TextIO.openOut filename
            val _ = saveToStream store (baseIri, stream)
        in
            TextIO.closeOut stream;
            OK
        end

    fun saveToFile store (baseIri, filename) =
        saveToFile' store (baseIri, filename)
        handle ex => SYSTEM_ERROR (exnMessage ex)
end

functor StoreAbbreviatingExporterFn
            (S: RDF_ABBREVIATING_SERIALISER
                    where type prefixTable = PrefixTable.t
                    where type matcher = Store.t)
        :> STORE_EXPORTER
               where type store = Store.t
               where type result = StoreExportBase.result
= struct

    open StoreExportBase
                     
    fun saveToStream store (baseIri, stream) =
        let val serialiser =
                S.new (baseIri, Store.getPrefixTable store, store) stream
        in
            S.finish (S.serialise (serialiser, Store.enumerate store));
            OK
        end

    fun saveToFile' store (baseIri, filename) =
        let val stream = TextIO.openOut filename
            val _ = saveToStream store (baseIri, stream)
        in
            TextIO.closeOut stream;
            OK
        end

    fun saveToFile store args =
        saveToFile' store args
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

    fun saveToStream store (baseIri, format, stream) =
        let open FileType
            val exporter = 
                case format of
                    TURTLE => TurtleExporter.saveToStream
                  | NTRIPLES => NTriplesExporter.saveToStream
                  | _ => raise Unsupported
        in
            (exporter store (baseIri, stream); OK)
            handle Unsupported => FORMAT_NOT_SUPPORTED
            handle ex => SYSTEM_ERROR (exnMessage ex)
        end

    val formatsSupported = [FileType.TURTLE, FileType.NTRIPLES]
            
end

structure StoreFileExporter
          :> STORE_FILE_EXPORTER
               where type store = Store.t
               where type result = StoreExportBase.result
= struct

    open StoreExportBase

    exception Unsupported

    fun saveToFile store (baseIri, filename) =
        let open FileType
            val exporter = 
                case formatOf filename of
                    TURTLE => TurtleExporter.saveToFile
                  | NTRIPLES => NTriplesExporter.saveToFile
                  | _ => raise Unsupported
        in
            (exporter store (baseIri, filename); OK)
            handle Unsupported => FORMAT_NOT_SUPPORTED
            handle ex => SYSTEM_ERROR (exnMessage ex)
        end

    val formatsSupported = [FileType.TURTLE, FileType.NTRIPLES]
            
    val extensionsSupported = 
        List.concat (map FileType.extensionsForFormat formatsSupported)
            
end

