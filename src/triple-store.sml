
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
				      
    type t = index_order * triple NodeMap.map NodeMap.map NodeMap.map

    fun new ix = (ix, NodeMap.empty)

    fun decompose (SPO, (subj,pred,obj)) = (subj,pred,obj)
      | decompose (POS, (subj,pred,obj)) = (pred,obj,subj)
      | decompose (OPS, (subj,pred,obj)) = (obj,pred,subj)

    fun recompose (SPO, (subj,pred,obj)) = (subj,pred,obj)
      | recompose (POS, (pred,obj,subj)) = (subj,pred,obj)
      | recompose (OPS, (obj,pred,subj)) = (subj,pred,obj)

    fun find_map (map, key) =
	getOpt (NodeMap.find (map, key), NodeMap.empty)

    fun add ((ix, map), triple) =
	let val (a, b, c) = decompose (ix, triple)
	    val m2 = find_map (map, a)
	    val m3 = find_map (m2, b)
	in
	    (ix, NodeMap.insert (map, a,
				 NodeMap.insert (m2, b,
						 NodeMap.insert (m3, c, triple))))
	end

    fun contains ((ix, map), triple) =
	let val (a, b, c) = decompose (ix, triple)
	in
	    case NodeMap.find (map, a) of
		NONE => false
	      | SOME m2 => case NodeMap.find (m2, b) of
			       NONE => false
			     | SOME m3 => isSome (NodeMap.find (m3, c))
	end					     

    fun remove ((ix, map), triple) = (* NB inefficient if triple is not in index *)
	let val (a, b, c) = decompose (ix, triple)
	    val m2 = find_map (map, a)
	    val m3 = find_map (m2, b)
	    val (cmap, _) = NodeMap.remove (m3, c)
			    handle NotFound => (m3, triple)
	in
	    (ix, NodeMap.insert (map, a,
				 NodeMap.insert (m2, b, cmap)))
	end

    fun match ((ix, m) : t, pattern) : triple list =
	let val (a, b, c) = decompose (ix, pattern)
	    fun find_in (x, mm) =
		case x of WILDCARD => NodeMap.listItems mm
			| KNOWN node => case NodeMap.find (mm, node) of
					    SOME value => [value]
					  | NONE => []
	    fun concatMap f l = List.concat (List.map f l)
	in
	    concatMap (fn bmap => concatMap (fn cmap => find_in (c, cmap))
					    (find_in (b, bmap)))
		      (find_in (a, m))
	end

    fun enumerate t = match (t, (WILDCARD, WILDCARD, WILDCARD))
	    
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
			       
structure TripleStore :> TRIPLE_STORE = struct

    datatype node = datatype RdfNode.node
    datatype patnode = datatype Index.patnode

    type triple = node * node * node
    type pattern = patnode * patnode * patnode

    structure StringMap = RedBlackMapFn (struct
                                          type ord_key = string
                                          val compare = String.compare
                                          end)
					   
    type t = {
	prefixes : string StringMap.map * string StringMap.map, (* fwd, reverse *)
	indexes : Index.t list
    }

    val empty =
	let open StringMap RdfStandardIRIs in {
	    prefixes =
	    (insert (insert (empty, "rdf", prefix_rdf), "xsd", prefix_xsd),
	     insert (insert (empty, prefix_rdf, "rdf"), prefix_xsd, "xsd")),
	    indexes = [ Index.new Index.SPO,
			Index.new Index.POS,
			Index.new Index.OPS ]
	}
	end

    structure IntMap = IntRedBlackMap

    fun any_index ({ indexes, ... } : t) = hd indexes (* when any index is ok *)
			   
    fun choose_index ({ indexes, ... } : t, pattern) =
	hd (IntMap.listItems
		(foldl (fn (ix, m) =>
			   IntMap.insert (m, Index.score (ix, pattern), ix))
		       IntMap.empty indexes))

    fun contains (store, triple) =
	Index.contains (any_index store, triple)

    fun map_indexes f ({ prefixes, indexes } : t) =
	{ prefixes = prefixes, indexes = map f indexes }
	  
    fun add (store, triple) =
	map_indexes (fn ix => Index.add (ix, triple)) store 

    fun remove (store, triple) =
	map_indexes (fn ix => Index.remove (ix, triple)) store

    fun enumerate store =
	Index.enumerate (any_index store)

    fun match (store, pattern) =
	Index.match (choose_index (store, pattern), pattern)

    fun add_prefix ({ prefixes, indexes } : t, prefix, expansion) =
	{ indexes = indexes,
	  prefixes = (StringMap.insert (#1 prefixes, prefix, expansion),
		      StringMap.insert (#2 prefixes, expansion, prefix)) }

    fun remove_from (map, key) =
	case StringMap.remove (map, key) of (map', _) => map'
							 handle NotFound => map
	    
    fun remove_prefix (store as { prefixes, indexes } : t, prefix) =
	case prefixes of
	    (forward, reverse) =>
	    case StringMap.find (forward, prefix) of
		NONE => store
	      | SOME expansion =>
		{ indexes = indexes,
		  prefixes = (remove_from (forward, prefix),
			      remove_from (reverse, expansion)) }

    fun contains_prefix ({ prefixes, ... } : t, prefix) =
	isSome (StringMap.find (#1 prefixes, prefix))
		    
    fun enumerate_prefixes ({ prefixes, ... } : t) =
	StringMap.listItemsi (#1 prefixes)

    fun expand (store, "a") = RdfStandardIRIs.iri_rdf_type
      | expand ({ prefixes, ... } : t, curie) =
	case String.fields (fn x => x = #":") curie of
	    [] => curie
	  | prefix::rest =>
	    case StringMap.find (#1 prefixes, prefix) of
		NONE => curie
	      | SOME expansion => expansion ^ (String.concatWith ":" rest)

    fun abbreviate ({ prefixes, ... } : t, iri) = 
	let val (_, reverse) = prefixes
	    fun prefix_of "" = (0, "")
	      | prefix_of name =
		let val len = String.size name in
		    case StringMap.find (reverse, name) of
			NONE => prefix_of (String.substring (name, 0, len - 1))
		      | SOME pfx => (len, pfx)
		end
	in
	    if iri = RdfStandardIRIs.iri_rdf_type then "a"
	    else
		case prefix_of iri of
		    (0, _) => iri
		  | (len, pfx) => pfx ^ ":" ^ (String.extract (iri, len, NONE))
	end

end
					    
