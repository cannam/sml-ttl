
structure PrefixTable :> PREFIX_TABLE = struct

    fun compare_backwards (s1, s2) =
        (* as with IRIs, we compare prefixes backwards because they
           often have common prefixes of their own *)
        let val n1 = String.size s1
            val n2 = String.size s2
            fun compare' ~1 = EQUAL
              | compare' n = 
                case Char.compare (String.sub (s1, n),
                                   String.sub (s2, n)) of
                    LESS => LESS
                  | GREATER => GREATER
                  | EQUAL => compare' (n - 1)
        in
            if n1 < n2 then LESS
            else if n1 > n2 then GREATER
            else compare' (n1 - 1)
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

    fun abbreviate ((_, reverse) : t, iri) = 
	let fun prefix_of "" = (0, "")
	      | prefix_of name =
		let val len = String.size name in
		    case StringMap.find (reverse, name) of
			NONE => prefix_of (String.substring (name, 0, len - 1))
		      | SOME pfx => (len, pfx)
		end
	in
	    if Iri.equals (iri, RdfStandardIRIs.iri_rdf_type) then SOME "a"
	    else
                let val iristr = Iri.toString iri in
		    case prefix_of iristr of
		        (0, _) => NONE
		      | (len, pfx) =>
                        SOME (pfx ^ ":" ^ (String.extract (iristr, len, NONE)))
                end
	end

end
