
signature INDEX = sig

    type t

    datatype node = datatype RdfNode.node
				    
    datatype patnode =
	     WILDCARD |
	     KNOWN of node

    type triple = node * node * node
    type pattern = patnode * patnode * patnode

    datatype index_order = SPO | POS | OPS

    val new : index_order -> t
    val add : t * triple -> t
    val contains : t * triple -> bool
    val remove : t * triple -> t
    val match : t * pattern -> triple list
    val enumerate : t -> triple list
    val score : t * triple -> int
    
end

structure Index :> INDEX = struct

    datatype node = datatype RdfNode.node
				    
    datatype patnode =
	     WILDCARD |
	     KNOWN of node

    type triple = node * node * node
    type pattern = patnode * patnode * patnode

    datatype index_order = SPO | POS | OPS

    structure NodeMap = RedBlackMapFn (struct
                                        type ord_key = node
                                        val compare = RdfNode.compare
                                        end)

    structure NodeSet = RedBlackSetFn (struct
					type ord_key = node
					val compare = RdfNode.compare
					end)
				      
    type t = index_order * NodeSet.set NodeMap.map NodeMap.map

    fun new ix = (ix, NodeMap.empty)

    fun decompose (SPO, (subj,pred,obj)) = (subj,pred,obj)
      | decompose (POS, (subj,pred,obj)) = (pred,obj,subj)
      | decompose (OPS, (subj,pred,obj)) = (obj,pred,subj)

    fun recompose (SPO, (subj,pred,obj)) = (subj,pred,obj)
      | recompose (POS, (pred,obj,subj)) = (subj,pred,obj)
      | recompose (OPS, (obj,pred,subj)) = (subj,pred,obj)

    fun find_map (map, key) =
	getOpt (NodeMap.find (map, key), NodeMap.empty)

    fun find_set (map, key) =
	getOpt (NodeMap.find (map, key), NodeSet.empty)
					       
    fun add ((ix, map), triple) =
	let val (a, b, c) = decompose (ix, triple)
	    val m2 = find_map (map, a)
	    val s  = find_set (m2, b)
	in
	    (ix, NodeMap.insert (map, a,
				 NodeMap.insert (m2, b,
						 NodeSet.add (s, c))))
	end

    fun contains ((ix, map), triple) =
	let val (a, b, c) = decompose (ix, triple)
	in
	    case NodeMap.find (map, a) of
		NONE => false
	      | SOME m2 => case NodeMap.find (m2, b) of
			       NONE => false
			     | SOME s => NodeSet.member (s, c)
	end					     

    fun remove ((ix, map), triple) = (* NB inefficient if triple is not in index *)
	let val (a, b, c) = decompose (ix, triple)
	    val m2 = find_map (map, a)
	    val s  = find_set (m2, b)
	in
	    (ix, NodeMap.insert (map, a,
				 NodeMap.insert (m2, b,
						 NodeSet.delete (s, c))))
	end

    fun match ((ix, map), pattern) =
	let val (a, b, c) = decompose (ix, pattern)
	    fun find_in (x, map) =
		case x of WILDCARD => NodeMap.listItemsi map
			| KNOWN node => case NodeMap.find (map, node) of
					    SOME m => [(node, m)]
					  | NONE => []
	in
	    
	end
	    
end
			       
		      
(* 
signature TRIPLE_STORE = sig

    type t
	     
    val add : t * triple -> unit
    val remove : t * triple -> bool
    val listItems : t -> triple list
    val contains : t * triple -> bool
    val match : t * pattern -> triple list
    val addPrefix : t * string * string -> unit
    val havePrefix : t * string -> bool
    val listPrefixes : t -> string list
    val expand : string -> node
    val abbreviate : node -> string (*!!!*)
	     
end

*)
