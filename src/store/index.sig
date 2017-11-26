
signature INDEX = sig

    type t

    datatype node = datatype RdfNode.node
				    
    type patnode = node option

    type triple = node * node * node
    type pattern = patnode * patnode * patnode

    datatype indexOrder = SPO | POS | OPS | SOP | PSO | OSP

    val new : indexOrder -> t
    val add : t * triple -> t
    val contains : t * triple -> bool
    val remove : t * triple -> t
    val foldlMatch : (triple * 'a -> 'a) -> 'a -> (t * pattern) -> 'a
    val score : t * pattern -> int
    val name : t -> string

    val nameOfOrder : indexOrder -> string
    val orderOfName : string -> indexOrder
                        
end
