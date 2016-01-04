
signature STORE_LOAD_BASE = sig

    structure Store : STORE
			  
    type base_iri = string

    datatype result = LOAD_ERROR of string | OK of Store.t

end
			  
signature STORE_LOADER = sig

    (* Signature for loaders that already know, or assume, what format
       is being loaded. *)

    include STORE_LOAD_BASE
                     
    val load_file : Store.t -> base_iri -> string -> result
    val load_stream : Store.t -> base_iri -> TextIO.instream -> result
    val load_string : Store.t -> base_iri -> string -> result
                     
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
    
    val load_file : Store.t -> base_iri -> string -> result
    val load_file_as_new_store : base_iri -> string -> result

end
                                    
signature STORE_EXPORTER = sig

    (* Signature for exporters that already know, or assume, what
       format is being saved to. *)

    structure Store : STORE

    val save_to_file : Store.t -> string -> unit
    val save_to_stream : Store.t -> TextIO.outstream -> unit

end

signature STORE_FILE_EXPORTER = sig

    (* Signature for exporters that determine the format to be
       exported based on file metadata, e.g. suffix. These therefore
       can only save to files, not arbitrary text streams, and will
       probably work by guessing and delegating to the appropriate
       STORE_EXPORTER internally. *)
    
    structure Store : STORE

    (* !!! todo: base iri *)
                     
    val save_to_file : Store.t -> string -> unit

end

