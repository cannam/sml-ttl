
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
    val enumerate : t -> triple list
    val match : t * pattern -> triple list
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

    val empty : t
    val add : t * triple -> t
    val contains : t * triple -> bool
    val remove : t * triple -> t
    val enumerate : t -> triple list
    val match : t * pattern -> triple list

    val add_prefix : t * string * string -> t
    val contains_prefix : t * string -> bool
    val remove_prefix : t * string -> t
    val enumerate_prefixes : t -> (string * string) list
    val expand : t * string -> string
    val abbreviate : t * string -> string
	     
end
			  
signature STORE_LOADER = sig

    structure Parser : RDF_STREAM_PARSER
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
			     
