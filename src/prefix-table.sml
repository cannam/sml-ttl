
structure PrefixTable :> PREFIX_TABLE = struct

    structure Comp = StringCompare(struct
                                    type str = string
                                    type ch = char
                                    val size = String.size
                                    val sub = String.sub
                                    val ch_compare = Char.compare
                                    end)
            
    structure StringMap = RedBlackMapFn (struct
                                          type ord_key = string
                                          val compare = Comp.compare_backwards
                                          end)

    type t = string StringMap.map * string StringMap.map * (string * string) list
    type iri = Iri.t

    (* The flat list is a list of (namespace, expansion) sorted in
       reverse order of expansion length. The first entry to match a
       prefix of a given IRI must be the longest prefix available for
       that IRI. This function sorted_flat maintains that ordering. *)
    fun sorted_flat flat =
        ListMergeSort.sort (fn ((_, e1), (_, e2)) =>
                               String.size e2 >= String.size e1)
                           flat
                        
    val empty : t = ( StringMap.empty, StringMap.empty, [] )

    fun remove_from (map, key) =
	case StringMap.remove (map, key) of (map', _) => map'
							 handle NotFound => map
	    
    fun remove (table as (forward, reverse, flat) : t, namespace) =
	case StringMap.find (forward, namespace) of
	    NONE => table
	  | SOME expansion =>
	    (remove_from (forward, namespace),
             remove_from (reverse, expansion),
             sorted_flat (List.filter (fn (ns, _) => ns <> namespace) flat))

    fun add (table : t, namespace, expansion) =
        (* remove the namespace first -- adding to a map will replace
           any existing key in the map, but that doesn't apply to our
           flat list *)
        let val (table as (forward, reverse, flat)) = remove (table, namespace) in
	    (StringMap.insert (forward, namespace, expansion),
	     StringMap.insert (reverse, expansion, namespace),
             sorted_flat ((namespace, expansion) :: flat))
        end

    fun from_prefixes prefixes =
        List.foldl (fn ((ns, e), tab) => add (tab, ns, e)) empty prefixes
                    
    fun contains (table : t, namespace) =
	isSome (StringMap.find (#1 table, namespace))
		    
    fun enumerate (table : t) =
        ListMergeSort.sort (fn ((ns1, _), (ns2, _)) => String.> (ns1, ns2))
                           (#3 table)

    fun expand (table, "a") = RdfStandardIRIs.iri_rdf_type
      | expand (table : t, curie) =
        Iri.fromString
	    (case String.fields (fn x => x = #":") curie of
	         [] => curie
	       | namespace::rest =>
	         case StringMap.find (#1 table, namespace) of
		     NONE => curie
	           | SOME expansion => expansion ^ (String.concatWith ":" rest))

    fun abbreviate_matching (table : t, iristr) =
        (* faster if there are few prefixes in the table? *)
        (* would be faster still if table stored wide strings *)
        case List.find (fn (_, e) => Comp.is_prefix (e, iristr)) (#3 table) of
            NONE => NONE
          | SOME (ns, e) => 
            SOME (ns ^ ":" ^ (String.extract (iristr, (String.size e), NONE)))
            
    fun abbreviate_reducing (table : t, iristr) =
        (* faster if there are lots of prefixes in the table and/or
           iri is short *)
	let fun match_prefix_of "" = (0, "")
	      | match_prefix_of name =
		let val len = String.size name in
		    case StringMap.find (#2 table, name) of
			NONE => match_prefix_of (String.substring (name, 0, len-1))
		      | SOME ns => (len, ns)
		end
	in
	    case match_prefix_of iristr of
		(0, _) => NONE
	      | (len, ns) =>
                SOME (ns ^ ":" ^ (String.extract (iristr, len, NONE)))
	end

    fun abbreviate (table, iri) = 
	if Iri.equals (iri, RdfStandardIRIs.iri_rdf_type) then SOME "a"
	else abbreviate_matching (table, Iri.toString iri)
            
end
