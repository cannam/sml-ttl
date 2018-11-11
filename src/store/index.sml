
structure Index :> INDEX = struct

    datatype node = datatype RdfNode.node

    type patnode = node option
    type triple = node * node * node
    type pattern = patnode * patnode * patnode

    datatype indexOrder = SPO | POS | OPS | SOP | PSO | OSP

    structure NodeTrie = ListMTrieFn(struct
                                      type t = node
                                      val compare = RdfNode.compare
                                      end)

    type t = indexOrder * NodeTrie.t

    fun nameOfOrder SPO = "spo" 
      | nameOfOrder POS = "pos"
      | nameOfOrder OPS = "ops"
      | nameOfOrder SOP = "sop"
      | nameOfOrder PSO = "pso"
      | nameOfOrder OSP = "osp"
            
    fun orderOfName "spo" = SPO
      | orderOfName "pos" = POS
      | orderOfName "ops" = OPS
      | orderOfName "sop" = SOP
      | orderOfName "pso" = PSO
      | orderOfName "osp" = OSP
      | orderOfName name = raise Fail ("Unknown index order name " ^ name)

    fun name (order, _) = nameOfOrder order

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

    fun foldlMatch f acc ((ord, ix) : t, pattern) =
        NodeTrie.foldlPatternMatch
            (fn (e, acc) => f (recompose (ord, e), acc))
            acc
            (ix, decompose (ord, pattern))
	    
    fun score ((ord, ix), pattern) = (* lower score is better *)
	case decompose (ord, pattern) of
	    NONE::NONE::_ => 10
	  | NONE::_ => 9
	  | _::NONE::_ => 4
	  | _ => 0
	    
end

signature INDEX_PICKER = sig

    type index
    type pattern
    
    val pickIndex : index list * pattern -> index
    
end
                               
functor IndexPickerFn (IX: INDEX) : INDEX_PICKER = struct
		
    type index = IX.t
    type pattern = IX.pattern
	   
    open IntRedBlackMap

    fun pickIndex (indexes : index list, pattern : pattern) : index =
	hd (listItems
		(List.foldl (fn (ord, m) =>
			        insert (m, IX.score (ord, pattern), ord))
		            empty indexes))

end

structure IndexPicker = IndexPickerFn(Index)

