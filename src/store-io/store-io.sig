
signature STORE_LOAD_BASE = sig

    type base_iri = BaseIri.t
    type store

    datatype result =
             (** Immediate failure due to unknown or unsupported format *)
             FORMAT_NOT_SUPPORTED |
             (** Error caught from system code, e.g. file not found *)
             SYSTEM_ERROR of string |
             (** Error produced during parsing, e.g. malformed syntax *)
             PARSE_ERROR of string |
             (** Successful load *)
             OK of store

end
			  
signature STORE_LOADER = sig

    (* STORE_LOADER and STORE_FILE_LOADER load RDF triples from a file
       or text stream into a queryable store.

       The STORE_LOADER signature is for loaders that load from a
       single format, and so don't need to be told what format to
       read. *)

    include STORE_LOAD_BASE
                
    val loadFile : store -> base_iri * string -> result
    val loadStream : store -> base_iri * TextIO.instream -> result
    val loadString : store -> base_iri * string -> result
                     
    val loadFileAsNewStore : base_iri * string -> result
    val loadStreamAsNewStore : base_iri * TextIO.instream -> result
    val loadStringAsNewStore : base_iri * string -> result

end
                             
signature STORE_FILE_LOADER = sig

    (* Signature for loaders that determine the format to be loaded
       based on file extension. These therefore can only load from
       files, not arbitrary text streams, and will probably work by
       guessing and delegating to the appropriate STORE_LOADER
       internally. *)

    include STORE_LOAD_BASE
    
    val loadFile : store -> base_iri * string -> result
    val loadFileAsNewStore : base_iri * string -> result
    val formatsSupported : FileType.format list
    val extensionsSupported : string list

end

signature STORE_EXPORT_BASE = sig
			  
    type base_iri = BaseIri.t
    type store

    datatype result =
             (** Immediate failure due to unknown or unsupported format *)
             FORMAT_NOT_SUPPORTED |
             (** Error caught from system code, e.g. file could not be opened *)
             SYSTEM_ERROR of string |
             (** Successful export *)
             OK

end
                                    
signature STORE_EXPORTER = sig

    (* STORE_EXPORTER and STORE_FILE_EXPORTER save RDF triples from a
       store to a file or text stream.

       The STORE_EXPORTER signature is for exporters that save to a
       single format, and so don't need to be told what format to save
       to.

       STORE_EXPORTER raises an exception on failure. *)

    include STORE_EXPORT_BASE

    val saveToFile : store -> base_iri * string -> result
    val saveToStream : store -> base_iri * TextIO.outstream -> result

end

signature STORE_STREAM_EXPORTER = sig

    (* Signature for exporters that determine the format to be
       exported based on a supplied format type.

       STORE_STREAM_EXPORTER raises an exception on failure. *)
    
    include STORE_EXPORT_BASE

    val saveToStream : store ->
                         base_iri * FileType.format * TextIO.outstream ->
                         result
    val formatsSupported : FileType.format list

end

signature STORE_FILE_EXPORTER = sig

    (* Signature for exporters that determine the format to be
       exported based on file extension. These therefore can only save
       to files, not arbitrary text streams, and will probably work by
       guessing and delegating to the appropriate STORE_EXPORTER
       internally.

       STORE_FILE_EXPORTER raises an exception on failure. *)
    
    include STORE_EXPORT_BASE

    val saveToFile : store -> base_iri * string -> result
    val formatsSupported : FileType.format list
    val extensionsSupported : string list

end

