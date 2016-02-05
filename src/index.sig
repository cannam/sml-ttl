
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
    val foldl_match : (triple * 'a -> 'a) -> 'a -> (t * pattern) -> 'a
    val score : t * pattern -> int
    val name : t -> string

    val name_of_order : index_order -> string
    val order_of_name : string -> index_order
                        
end
