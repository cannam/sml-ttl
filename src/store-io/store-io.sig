
signature STORE_LOAD_BASE = sig
			  
    type base_iri = string
    datatype result = LOAD_ERROR of string | OK of Store.t

end
			  
signature STORE_LOADER = sig

    (* STORE_LOADER and STORE_FILE_LOADER load RDF triples from a file
       or text stream into a queryable store.

       The STORE_LOADER signature is for loaders that load from a
       single format, and so don't need to be told what format to
       read. *)

    include STORE_LOAD_BASE
    type store
                     
    val load_file : store -> base_iri -> string -> result
    val load_stream : store -> base_iri -> TextIO.instream -> result
    val load_string : store -> base_iri -> string -> result
                     
    val load_file_as_new_store : base_iri -> string -> result
    val load_stream_as_new_store : base_iri -> TextIO.instream -> result
    val load_string_as_new_store : base_iri -> string -> result

end
                             
signature STORE_FILE_LOADER = sig

    (* Signature for loaders that determine the format to be loaded
       based on file metadata, e.g. suffix. These therefore can only
       load from files, not arbitrary text streams, and will probably
       work by guessing and delegating to the appropriate STORE_LOADER
       internally. *)

    include STORE_LOAD_BASE
    type store
    
    val load_file : store -> base_iri -> string -> result
    val load_file_as_new_store : base_iri -> string -> result

end
                                    
signature STORE_EXPORTER = sig

    (* STORE_EXPORTER and STORE_FILE_EXPORTER save RDF triples from a
       store to a file or text stream.

       The STORE_EXPORTER signature is for exporters that save to a
       single format, and so don't need to be told what format to save
       to.

       STORE_EXPORTER raises an exception on failure. *)

    type store

    val save_to_file : store -> string -> unit
    val save_to_stream : store -> TextIO.outstream -> unit

end

signature STORE_FILE_EXPORTER = sig

    (* Signature for exporters that determine the format to be
       exported based on file metadata, e.g. suffix. These therefore
       can only save to files, not arbitrary text streams, and will
       probably work by guessing and delegating to the appropriate
       STORE_EXPORTER internally.

       STORE_FILE_EXPORTER raises an exception on failure. *)
    
    type store

    (* !!! todo: base iri? *)
                     
    val save_to_file : store -> string -> unit

end

