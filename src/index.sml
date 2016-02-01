
structure Index :> INDEX = struct

    datatype node = datatype RdfNode.node
				    
    datatype patnode =
	     WILDCARD |
	     KNOWN of node

    type triple = node * node * node
    type pattern = patnode * patnode * patnode

    datatype index_order = SPO | POS | OPS | SOP | PSO | OSP
							     
    structure NodeMap = RedBlackMapFn (struct
                                        type ord_key = node
                                        val compare = RdfNode.compare
                                        end)

    type t = index_order * triple NodeMap.map NodeMap.map NodeMap.map

    fun name (SPO, _) = "spo" 
      | name (POS, _) = "pos"
      | name (OPS, _) = "ops"
      | name (SOP, _) = "sop"
      | name (PSO, _) = "pso"
      | name (OSP, _) = "osp"

    fun new ix = (ix, NodeMap.empty)

    fun decompose (SPO, (subj,pred,obj)) = (subj,pred,obj)
      | decompose (POS, (subj,pred,obj)) = (pred,obj,subj)
      | decompose (OPS, (subj,pred,obj)) = (obj,pred,subj)
      | decompose (SOP, (subj,pred,obj)) = (subj,obj,pred)
      | decompose (PSO, (subj,pred,obj)) = (pred,subj,obj)
      | decompose (OSP, (subj,pred,obj)) = (obj,subj,pred)

    fun recompose (SPO, (subj,pred,obj)) = (subj,pred,obj)
      | recompose (POS, (pred,obj,subj)) = (subj,pred,obj)
      | recompose (OPS, (obj,pred,subj)) = (subj,pred,obj)
      | recompose (SOP, (subj,obj,pred)) = (subj,pred,obj)
      | recompose (PSO, (pred,subj,obj)) = (subj,pred,obj)
      | recompose (OSP, (obj,subj,pred)) = (subj,pred,obj)

    fun find_map (m, key) =
        getOpt (NodeMap.find (m, key), NodeMap.empty)

    fun add ((ix, m) : t, triple) =
	let val (a, b, c) = decompose (ix, triple)
	    val m2 = find_map (m, a)
	    val m3 = find_map (m2, b)
	in 
	    (ix, NodeMap.insert (m, a,
		                 NodeMap.insert (m2, b,
                                                 NodeMap.insert (m3, c, triple))))
	end

    fun contains ((ix, m) : t, triple) =
	let val (a, b, c) = decompose (ix, triple)
	in
	    case NodeMap.find (m, a) of
		NONE => false
	      | SOME m2 => case NodeMap.find (m2, b) of
			       NONE => false
			     | SOME m3 => isSome (NodeMap.find (m3, c))
	end					     

    fun remove ((ix, map), triple) =
	let val (a, b, c) = decompose (ix, triple)
	    val m2 = find_map (map, a)
	    val m3 = find_map (m2, b)
	    val (cmap, _) = NodeMap.remove (m3, c)
			    handle NotFound => (m3, triple)
	in
	    (ix, NodeMap.insert (map, a,
				 NodeMap.insert (m2, b, cmap)))
	end

    fun matching (p, m) =
        case p of WILDCARD => NodeMap.listItems m
                | KNOWN node => case NodeMap.find (m, node) of
                                    SOME value => [value]
                                  | NONE => []

    fun fold_match f acc ((ix, m) : t, pattern) =
	let val (a, b, c) = decompose (ix, pattern)
        in
            foldl (fn (am, aa) =>
                      foldl (fn (bm, ba) =>
                                foldl f ba (matching (c, bm)))
                            aa (matching (b, am)))
                  acc (matching (a, m))
        end
	    
    fun score ((ix, map), pattern) = (* lower score is better *)
	let val (a, b, _) = decompose (ix, pattern)
	in
	    case (a, b) of
		(WILDCARD, WILDCARD) => 10
	      | (WILDCARD, _) => 9
	      | (_, WILDCARD) => 4
	      | (_, _) => 0
	end
	    
end

signature INDEX_PICKER = sig

    type index
    type pattern
    
    val pick_index : index list * pattern -> index
    
end
                               
structure IndexPicker : INDEX_PICKER = struct
		
    type index = Index.t
    type pattern = Index.pattern
	   
    open IntRedBlackMap

    fun pick_index (indexes, pattern) =
	hd (listItems
		(List.foldl (fn (ix, m) =>
			        insert (m, Index.score (ix, pattern), ix))
		            empty indexes))

end
