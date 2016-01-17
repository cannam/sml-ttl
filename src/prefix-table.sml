
structure PrefixTable :> PREFIX_TABLE = struct

    (* as with IRIs, we compare prefixes backwards because they
       often have common prefixes of their own *)
    fun compare_prefix_backwards (s1, s2, 0) = EQUAL
      | compare_prefix_backwards (s1, s2, plen) =
        let val m = plen - 1
        in
            case Char.compare (String.sub (s1, m),
                               String.sub (s2, m)) of
                LESS => LESS
              | GREATER => GREATER
              | EQUAL => compare_prefix_backwards (s1, s2, m)
        end

    fun compare_backwards (s1, s2) =
        let val n1 = String.size s1
            val n2 = String.size s2
        in
            if n1 < n2 then LESS
            else if n1 > n2 then GREATER
            else compare_prefix_backwards (s1, s2, n1)
        end

    fun is_prefix (s1, s2) =
        let val n1 = String.size s1
            val n2 = String.size s2
        in
            if n1 > n2 then false
            else compare_prefix_backwards (s1, s2, n1) = EQUAL
        end
            
    structure StringMap = RedBlackMapFn (struct
                                          type ord_key = string
                                          val compare = compare_backwards
                                          end)

    type t = string StringMap.map * string StringMap.map (* fwd, reverse *)
    type iri = Iri.t
                                           
    val empty = (StringMap.empty, StringMap.empty)

    fun add (prefixes : t, prefix, expansion) =
	(StringMap.insert (#1 prefixes, prefix, expansion),
	 StringMap.insert (#2 prefixes, expansion, prefix))

    fun from_prefixes prefixes =
        List.foldl (fn ((p, e), tab) => add (tab, p, e)) empty prefixes
                    
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

    fun abbreviate_matching ((forward, _) : t, iristr) =
        (* faster if there are few prefixes in the table? *)
        (* would be faster still if table stored wide strings *)
        (* or if we had a separate list of just the expansions! *)
        let val matching = StringMap.filter (fn v => is_prefix (v, iristr)) forward
            val sorted =
                if StringMap.isEmpty matching then []
                else ListMergeSort.sort (fn ((_, e1), (_, e2)) =>
                                            String.size e2 >= String.size e1)
                                        (StringMap.listItemsi matching)
        in
            case sorted of
                [] => NONE
              | (p, e)::rest =>
                SOME (p ^ ":" ^ (String.extract (iristr, (String.size e), NONE)))
        end
            
    fun abbreviate_reducing ((_, reverse) : t, iristr) =
        (* faster if there are lots of prefixes in the table and/or
           iri is short *)
	let fun match_prefix_of "" = (0, "")
	      | match_prefix_of name =
		let val len = String.size name in
		    case StringMap.find (reverse, name) of
			NONE => match_prefix_of (String.substring (name, 0, len-1))
		      | SOME pfx => (len, pfx)
		end
	in
	    case match_prefix_of iristr of
		(0, _) => NONE
	      | (len, pfx) =>
                SOME (pfx ^ ":" ^ (String.extract (iristr, len, NONE)))
	end

    fun abbreviate (table, iri) = 
	if Iri.equals (iri, RdfStandardIRIs.iri_rdf_type) then SOME "a"
	else abbreviate_matching (table, Iri.toString iri)
            
end
