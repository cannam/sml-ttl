		      
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
    val foldl_match : (triple * 'a -> 'a) -> 'a -> (t * pattern) -> 'a
    val foldl : (triple * 'a -> 'a) -> 'a -> t -> 'a
    val enumerate : t -> triple list

    val add_prefix : t * string * string -> t
    val contains_prefix : t * string -> bool
    val remove_prefix : t * string -> t
    val enumerate_prefixes : t -> (string * string) list
    val expand : t * string -> iri
    val abbreviate : t * iri -> (string * string) option (* NONE if no prefix matches *)
	     
end

