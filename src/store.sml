
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

structure Index :> INDEX = struct

    datatype node = datatype RdfNode.node
				    
    datatype patnode =
	     WILDCARD |
	     KNOWN of node

    type triple = node * node * node
    type pattern = patnode * patnode * patnode

    datatype index_order = SPO | POS | OPS | SOP | PSO | OSP

    fun name SPO = "spo" 
      | name POS = "pos"
      | name OPS = "ops"
      | name SOP = "sop"
      | name PSO = "pso"
      | name OSP = "osp"
							     
    structure NodeMap = RedBlackMapFn (struct
                                        type ord_key = node
                                        val compare = RdfNode.compare
                                        end)

    type t = index_order * triple NodeMap.map NodeMap.map NodeMap.map

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

structure IndexPicker = struct
			   
    open IntRedBlackMap

    fun pick_index (indexes, pattern) =
	hd (listItems
		(List.foldl (fn (ix, m) =>
			        insert (m, Index.score (ix, pattern), ix))
		            empty indexes))

end

signature PREFIX_TABLE = sig

    type t
    type iri = Iri.t

    val empty : t
    val add : t * string * string -> t
    val contains : t * string -> bool
    val remove : t * string -> t
    val enumerate : t -> (string * string) list

    val expand : t * string -> iri
    val abbreviate : t * iri -> string

end

structure PrefixTable :> PREFIX_TABLE = struct

    structure StringMap = RedBlackMapFn (struct
                                          type ord_key = string
                                          val compare = String.compare
                                          end)

    type t = string StringMap.map * string StringMap.map (* fwd, reverse *)
    type iri = Iri.t
                                           
    val empty = 
        let open StringMap RdfStandardIRIs
        in
	    (insert (insert (empty, "rdf", prefix_rdf), "xsd", prefix_xsd),
	     insert (insert (empty, prefix_rdf, "rdf"), prefix_xsd, "xsd"))
        end

    fun add (prefixes : t, prefix, expansion) =
	(StringMap.insert (#1 prefixes, prefix, expansion),
	 StringMap.insert (#2 prefixes, expansion, prefix))

    fun remove_from (map, key) =
	case StringMap.remove (map, key) of (map', _) => map'
							 handle NotFound => map
	    
    fun remove (prefixes : t, prefix) =
	case prefixes of
	    (forward, reverse) =>
	    case StringMap.find (forward, prefix) of
		NONE => prefixes
	      | SOME expansion =>
		  (remove_from (forward, prefix),
		   remove_from (reverse, expansion))

    fun contains (prefixes : t, prefix) =
	isSome (StringMap.find (#1 prefixes, prefix))
		    
    fun enumerate (prefixes : t) =
	StringMap.listItemsi (#1 prefixes)

    fun expand (prefixes, "a") = RdfStandardIRIs.iri_rdf_type
      | expand (prefixes : t, curie) =
        Iri.fromString
	    (case String.fields (fn x => x = #":") curie of
	         [] => curie
	       | prefix::rest =>
	         case StringMap.find (#1 prefixes, prefix) of
		     NONE => curie
	           | SOME expansion => expansion ^ (String.concatWith ":" rest))

    fun abbreviate ((_, reverse) : t, iri) = 
	let fun prefix_of "" = (0, "")
	      | prefix_of name =
		let val len = String.size name in
		    case StringMap.find (reverse, name) of
			NONE => prefix_of (String.substring (name, 0, len - 1))
		      | SOME pfx => (len, pfx)
		end
	in
	    if Iri.equals (iri, RdfStandardIRIs.iri_rdf_type) then "a"
	    else
                let val iristr = Iri.toString iri in
		    case prefix_of iristr of
		        (0, _) => iristr
		      | (len, pfx) =>
                        pfx ^ ":" ^ (String.extract (iristr, len, NONE))
                end
	end

end
                             
structure Store :> STORE = struct

    datatype node = datatype RdfNode.node
    datatype patnode = datatype Index.patnode

    type triple = node * node * node
    type pattern = patnode * patnode * patnode
    type iri = Iri.t
					   
    type t = {
	prefixes : PrefixTable.t,
	indexes : Index.t list
    }

    val empty = {
	prefixes = PrefixTable.empty,
	indexes = [ Index.new Index.SPO,
		    Index.new Index.POS,
		    Index.new Index.OPS ]
    }

    fun any_index ({ indexes, ... } : t) = hd indexes (* when any index is ok *)

    fun contains (store, triple) =
	Index.contains (any_index store, triple)

    fun map_indexes f ({ prefixes, indexes } : t) =
	{ prefixes = prefixes, indexes = map f indexes }
	  
    fun add (store, triple) =
	map_indexes (fn ix => Index.add (ix, triple)) store 

    fun remove (store, triple) =
	map_indexes (fn ix => Index.remove (ix, triple)) store

    fun fold_match f acc ({ prefixes, indexes } : t, pattern) =
        Index.fold_match f acc
                         (IndexPicker.pick_index (indexes, pattern), pattern)

    fun foldl f acc store =
        Index.fold_match f acc
                         (any_index store, (WILDCARD, WILDCARD, WILDCARD))
                         
    val match = fold_match (op::) []

    val enumerate = foldl (op::) []

    fun add_prefix ({ prefixes, indexes } : t, prefix, expansion) =
        { prefixes = PrefixTable.add (prefixes, prefix, expansion),
          indexes = indexes }
	    
    fun remove_prefix (store as { prefixes, indexes } : t, prefix) =
        { prefixes = PrefixTable.remove (prefixes, prefix),
          indexes = indexes }

    fun contains_prefix ({ prefixes, ... } : t, prefix) =
        PrefixTable.contains (prefixes, prefix)
		    
    fun enumerate_prefixes ({ prefixes, ... } : t) =
        PrefixTable.enumerate prefixes

    fun expand ({ prefixes, ... } : t, curie) =
        PrefixTable.expand (prefixes, curie)

    fun abbreviate ({ prefixes, ... } : t, iri) =
        PrefixTable.abbreviate (prefixes, iri)

end

structure StoreLoadBase : STORE_LOAD_BASE = struct

    structure Store = Store
			  
    datatype result = LOAD_ERROR of string | OK of Store.t
			  
    type base_iri = string

end
                                            
functor StoreStreamLoaderFn (P: RDF_STREAM_PARSER) : STORE_FORMAT_LOADER = struct

    open StoreLoadBase

    fun load_stream store iri stream : result =
	let fun parse' acc f =
		case f () of
		    P.END_OF_STREAM => OK acc
		  | P.PARSE_ERROR err => LOAD_ERROR err
		  | P.PARSE_OUTPUT ({ prefixes, triples }, f') =>
		    parse'
			(foldl (fn (triple, s) => Store.add (s, triple))
			       (foldl (fn ((pfx, exp), s) =>
					  Store.add_prefix (s, pfx, exp))
				      acc prefixes)
			       triples)
			f'
	in
	    parse' Store.empty (fn () => P.parse iri stream)
	end
								  
    fun load_string store iri string =
        let val stream = TextIO.openString string
            val result = load_stream store iri stream
        in
            TextIO.closeIn stream;
            result
        end

    fun load_file store iri filename =
        let val stream = TextIO.openIn filename
            val result = load_stream store iri stream
        in
            TextIO.closeIn stream;
            result
        end

    val load_stream_as_new_store = load_stream Store.empty
    val load_string_as_new_store = load_string Store.empty
    val load_file_as_new_store = load_file Store.empty
			
end

functor StoreStreamSaverFn (S: RDF_STREAM_SERIALISER) : STORE_FORMAT_SAVER = struct

    structure Store = Store

    fun save_to_stream store stream =
        let val serialiser = S.new stream
        in
            Store.foldl (fn (t, s) => S.serialise (s, S.TRIPLE t))
                        (List.foldl (fn (p, s) => S.serialise (s, S.PREFIX p))
                                    serialiser
                                    (Store.enumerate_prefixes store))
                        store;
            ()
        end
            
    fun save_to_file store filename =
        let val stream = TextIO.openOut filename
            val _ = save_to_stream store stream
        in
            TextIO.closeOut stream
        end
			  
end

structure TurtleLoader = StoreStreamLoaderFn(TurtleStreamParser)
					    
structure NTriplesSaver = StoreStreamSaverFn(NTriplesSerialiser)

structure StoreFileLoader : STORE_LOADER = struct

    open StoreLoadBase

    fun load_file store iri filename =
        let val extension =
                case String.tokens (fn x => x = #".") filename of
                    [] => ""
                  | bits => hd (rev bits)
        in
            (case extension of
                 "ttl" => TurtleLoader.load_file 
               | "n3" => TurtleLoader.load_file
               | "ntriples" => TurtleLoader.load_file
               | other => raise Fail ("Unknown or unsupported file extension \""
                                      ^ extension ^ "\""))
                store iri filename
        end

    val load_file_as_new_store = load_file Store.empty

end

structure StoreFileSaver : STORE_SAVER = struct

    structure Store = Store

    fun save_to_file store filename =
        let val extension =
                case String.tokens (fn x => x = #".") filename of
                    [] => ""
                  | bits => hd (rev bits)
        in
            (case extension of
                 "ntriples" => NTriplesSaver.save_to_file
               | other => raise Fail ("Unknown or unsupported file extension \""
                                      ^ extension ^ "\""))
                store filename
        end
                                  
end

