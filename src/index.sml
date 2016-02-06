
structure Index :> INDEX = struct

    datatype node = datatype RdfNode.node
				    
    datatype patnode =
	     WILDCARD |
	     KNOWN of node

    type triple = node * node * node
    type pattern = patnode * patnode * patnode

    datatype index_order = SPO | POS | OPS | SOP | PSO | OSP

    structure NodeTrie = TrieFn(struct
                                 type t = node
                                 val compare = RdfNode.compare
                                 end)

    type t = index_order * NodeTrie.t

    fun name_of_order SPO = "spo" 
      | name_of_order POS = "pos"
      | name_of_order OPS = "ops"
      | name_of_order SOP = "sop"
      | name_of_order PSO = "pso"
      | name_of_order OSP = "osp"
            
    fun order_of_name "spo" = SPO
      | order_of_name "pos" = POS
      | order_of_name "ops" = OPS
      | order_of_name "sop" = SOP
      | order_of_name "pso" = PSO
      | order_of_name "osp" = OSP
      | order_of_name name = raise Fail ("Unknown index order name " ^ name)

    fun name (order, _) = name_of_order order

    fun new ord = (ord, NodeTrie.empty)

    fun decompose (SPO, (subj,pred,obj)) = [subj,pred,obj]
      | decompose (POS, (subj,pred,obj)) = [pred,obj,subj]
      | decompose (OPS, (subj,pred,obj)) = [obj,pred,subj]
      | decompose (SOP, (subj,pred,obj)) = [subj,obj,pred]
      | decompose (PSO, (subj,pred,obj)) = [pred,subj,obj]
      | decompose (OSP, (subj,pred,obj)) = [obj,subj,pred]

    fun recompose (SPO, [subj,pred,obj]) = (subj,pred,obj)
      | recompose (POS, [pred,obj,subj]) = (subj,pred,obj)
      | recompose (OPS, [obj,pred,subj]) = (subj,pred,obj)
      | recompose (SOP, [subj,obj,pred]) = (subj,pred,obj)
      | recompose (PSO, [pred,subj,obj]) = (subj,pred,obj)
      | recompose (OSP, [obj,subj,pred]) = (subj,pred,obj)
      | recompose (_, _) = raise Fail "Wrong number of arguments to recompose"

    fun add ((ord, ix) : t, triple) =
        (ord, NodeTrie.add (ix, decompose (ord, triple)))

    fun contains ((ord, ix) : t, triple) =
        NodeTrie.contains (ix, decompose (ord, triple))

    fun remove ((ord, ix), triple) =
        (ord, NodeTrie.remove (ix, decompose (ord, triple)))

    datatype 'a pfx_state = WILDCARD_FOUND of 'a | ALL_KNOWN of 'a
            
    fun prefix (ord, pattern) =
        let val decomposed = decompose (ord, pattern)
        in case List.foldl (fn (_, WILDCARD_FOUND k) => WILDCARD_FOUND k
                           | (KNOWN n, ALL_KNOWN k) => ALL_KNOWN (k @ [n])
                           | (WILDCARD, ALL_KNOWN k) => WILDCARD_FOUND k)
                           (ALL_KNOWN [])
                           decomposed
            of
               WILDCARD_FOUND k => k
             | ALL_KNOWN k => k
        end
            
    fun foldl_match f acc ((ord, ix) : t, pattern) =
        let val pfx = prefix (ord, pattern)
        in
            (*!!! This is not right -- if the pattern goes
            e.g. KNOWN,KNOWN,WILDCARD we're OK, but if there's a
            WILDCARD before a KNOWN [which is a bad case for any
            index] we need to filter the results of the foldl, because
            it's only a match on the KNOWN prefix *)
            
            NodeTrie.foldl_prefix_match
                (fn (e, acc) => f (recompose (ord, e), acc))
                acc
                (ix, pfx)
        end
	    
    fun score ((ord, ix), pattern) = (* lower score is better *)
	case decompose (ord, pattern) of
	    WILDCARD::WILDCARD::_ => 10
	  | WILDCARD::_ => 9
	  | _::WILDCARD::_ => 4
	  | _ => 0
	    
end

signature INDEX_PICKER = sig

    type index
    type pattern
    
    val pick_index : index list * pattern -> index
    
end
                               
functor IndexPickerFn (IX: INDEX) : INDEX_PICKER = struct
		
    type index = IX.t
    type pattern = IX.pattern
	   
    open IntRedBlackMap

    fun pick_index (indexes : index list, pattern : pattern) : index =
	hd (listItems
		(List.foldl (fn (ord, m) =>
			        insert (m, IX.score (ord, pattern), ord))
		            empty indexes))

end

structure IndexPicker = IndexPickerFn(Index)

