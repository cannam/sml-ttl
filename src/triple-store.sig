
signature INDEX = sig

    type t

    datatype node = datatype RdfNode.node
				    
    datatype patnode =
	     WILDCARD |
	     KNOWN of node

    type triple = node * node * node
    type pattern = patnode * patnode * patnode

    datatype index_order = SPO | POS | OPS | SOP | PSO | OSP

    val new : index_order -> t
    val add : t * triple -> t
    val contains : t * triple -> bool
    val remove : t * triple -> t
    val fold_match : (triple * 'a -> 'a) -> 'a -> (t * pattern) -> 'a
    val score : t * pattern -> int
    
end
		      
signature TRIPLE_STORE = sig

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
			  
signature STORE_LOADER = sig

    structure Store : TRIPLE_STORE

    datatype result = LOAD_ERROR of string | OK of Store.t
			  
    type base_iri = string
                     
    val load_stream : Store.t -> base_iri -> TextIO.instream -> result
    val load_string : Store.t -> base_iri -> string -> result
    val load_file : Store.t -> base_iri -> string -> result
                     
    val load_stream_as_new_store : base_iri -> TextIO.instream -> result
    val load_string_as_new_store : base_iri -> string -> result
    val load_file_as_new_store : base_iri -> string -> result

end

signature STORE_SAVER = sig

    structure Store : TRIPLE_STORE

    val save_to_stream : Store.t -> TextIO.outstream -> unit
    val save_to_file : Store.t -> string -> unit

end

                            
