
signature STORE_LOAD_BASE = sig

    (*!!! surely should be an Iri.t ? see parse-serialise similarly *)
    type base_iri = string

    datatype result =
             (** Immediate failure due to unknown or unsupported format *)
             FORMAT_NOT_SUPPORTED |
             (** Error caught from system code, e.g. file not found *)
             SYSTEM_ERROR of string |
             (** Error produced during parsing, e.g. malformed syntax *)
             PARSE_ERROR of string |
             (** Successful load *)
             OK of Store.t

    type store

end
			  
signature STORE_LOADER = sig

    (* STORE_LOADER and STORE_FILE_LOADER load RDF triples from a file
       or text stream into a queryable store.

       The STORE_LOADER signature is for loaders that load from a
       single format, and so don't need to be told what format to
       read. *)

    include STORE_LOAD_BASE

(*!!! elsewhere, i.e. in parse-serialise, this is (base_iri * string) *)
                
    val load_file : store -> base_iri -> string -> result
    val load_stream : store -> base_iri -> TextIO.instream -> result
    val load_string : store -> base_iri -> string -> result
                     
    val load_file_as_new_store : base_iri -> string -> result
    val load_stream_as_new_store : base_iri -> TextIO.instream -> result
    val load_string_as_new_store : base_iri -> string -> result

end
                             
signature STORE_FILE_LOADER = sig

    (* Signature for loaders that determine the format to be loaded
       based on file extension. These therefore can only load from
       files, not arbitrary text streams, and will probably work by
       guessing and delegating to the appropriate STORE_LOADER
       internally. *)

    include STORE_LOAD_BASE
    
    val load_file : store -> base_iri -> string -> result
    val load_file_as_new_store : base_iri -> string -> result
    val formats_supported : FileType.format list
    val extensions_supported : string list

end

signature STORE_EXPORT_BASE = sig
			  
    (*!!! surely should be an Iri.t ? see parse-serialise similarly *)
    type base_iri = string

    datatype result =
             (** Immediate failure due to unknown or unsupported format *)
             FORMAT_NOT_SUPPORTED |
             (** Error caught from system code, e.g. file could not be opened *)
             SYSTEM_ERROR of string |
             (** Successful export *)
             OK

    type store

end
                                    
signature STORE_EXPORTER = sig

    (* STORE_EXPORTER and STORE_FILE_EXPORTER save RDF triples from a
       store to a file or text stream.

       The STORE_EXPORTER signature is for exporters that save to a
       single format, and so don't need to be told what format to save
       to.

       STORE_EXPORTER raises an exception on failure. *)

    include STORE_EXPORT_BASE

    val save_to_file : store -> base_iri -> string -> result
    val save_to_stream : store -> base_iri -> TextIO.outstream -> result

end

signature STORE_STREAM_EXPORTER = sig

    (* Signature for exporters that determine the format to be
       exported based on a supplied format type.

       STORE_STREAM_EXPORTER raises an exception on failure. *)
    
    include STORE_EXPORT_BASE

    val save_to_stream : store -> base_iri -> FileType.format * TextIO.outstream -> result
    val formats_supported : FileType.format list

end

signature STORE_FILE_EXPORTER = sig

    (* Signature for exporters that determine the format to be
       exported based on file extension. These therefore can only save
       to files, not arbitrary text streams, and will probably work by
       guessing and delegating to the appropriate STORE_EXPORTER
       internally.

       STORE_FILE_EXPORTER raises an exception on failure. *)
    
    include STORE_EXPORT_BASE

    val save_to_file : store -> base_iri -> string -> result
    val formats_supported : FileType.format list
    val extensions_supported : string list

end

