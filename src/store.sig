		      
signature STORE = sig

    type t

    datatype node = datatype RdfNode.node
				    
    datatype patnode =
	     WILDCARD |
	     KNOWN of node

    type triple = node * node * node
    type pattern = patnode * patnode * patnode
    type iri = Iri.t

    val empty : t
    val add : t * triple -> t
    val contains : t * triple -> bool
    val remove : t * triple -> t
    val match : t * pattern -> triple list
    val fold_match : (triple * 'a -> 'a) -> 'a -> (t * pattern) -> 'a
    val foldl : (triple * 'a -> 'a) -> 'a -> t -> 'a
    val enumerate : t -> triple list

    val add_prefix : t * string * string -> t
    val contains_prefix : t * string -> bool
    val remove_prefix : t * string -> t
    val enumerate_prefixes : t -> (string * string) list
    val expand : t * string -> iri
    val abbreviate : t * iri -> string
	     
end

signature STORE_LOAD_BASE = sig

    structure Store : STORE
			  
    type base_iri = string

    datatype result = LOAD_ERROR of string | OK of Store.t

end
                             
signature STORE_LOADER = sig

    (* Signature for loaders that determine the format to be loaded
       based on file metadata, e.g. suffix. These therefore can only
       load from files, not arbitrary text streams. *)

    include STORE_LOAD_BASE
    
    val load_file : Store.t -> base_iri -> string -> result
    val load_file_as_new_store : base_iri -> string -> result

end
			  
signature STORE_FORMAT_LOADER = sig

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

signature STORE_SAVER = sig

    (* Signature for loaders that determine the format to be loaded
       based on filename, e.g. suffix. These therefore can only save
       to files, not arbitrary text streams. *)
    
    structure Store : STORE

    (* !!! todo: base iri *)
                     
    val save_to_file : Store.t -> string -> unit

end
                                    
signature STORE_FORMAT_SAVER = sig

    (* Signature for savers that already know, or assume, what format
       is being saved to. *)

    structure Store : STORE

    val save_to_file : Store.t -> string -> unit
    val save_to_stream : Store.t -> TextIO.outstream -> unit

end

                            
