
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

    type t = string StringMap.map * string StringMap.map
    type iri = Iri.t
                        
    val empty : t = (StringMap.empty, StringMap.empty)

    fun remove_from (map, key) =
	case StringMap.remove (map, key) of (map', _) => map'
							 handle NotFound => map
	    
    fun remove (table as (forward, reverse) : t, namespace) =
	case StringMap.find (forward, namespace) of
	    NONE => table
	  | SOME expansion =>
	    (remove_from (forward, namespace),
             remove_from (reverse, expansion))

    fun add ((forward, reverse) : t, namespace, expansion) =
	(StringMap.insert (forward, namespace, expansion),
	 StringMap.insert (reverse, expansion, namespace))

    fun from_prefixes prefixes =
        List.foldl (fn ((ns, e), tab) => add (tab, ns, e)) empty prefixes
                    
    fun contains (table : t, namespace) =
	isSome (StringMap.find (#1 table, namespace))
		    
    fun enumerate (table : t) = StringMap.listItemsi (#1 table)

    fun expand ((forward, _) : t, curie) =
        Iri.fromString
	    (case String.fields (fn x => x = #":") curie of
	         [] => curie
	       | namespace::rest =>
	         case StringMap.find (forward, namespace) of
		     NONE => curie
	           | SOME expansion => expansion ^ (String.concatWith ":" rest))

    fun abbreviate_matching ((_, reverse) : t, iristr) =
        (* faster if there are few prefixes in the table? *)
        (* This depends on the keys of reverse being ordered with short
           strings before long ones (since we're looking for the longest
           prefix) *)
        StringMap.foldli
            (fn (e, ns, acc) =>
                if Comp.is_prefix (e, iristr)
                then SOME (ns, String.extract (iristr, String.size e, NONE))
                else acc)
            NONE reverse

    fun abbreviate_reducing ((_, reverse) : t, iristr) =
        (* faster if there are lots of prefixes in the table and/or
           iri is short *)
	let fun match_prefix_of "" = (0, "")
	      | match_prefix_of name =
		let val len = String.size name in
		    case StringMap.find (reverse, name) of
			NONE => match_prefix_of (String.substring (name, 0, len-1))
		      | SOME ns => (len, ns)
		end
	in
	    case match_prefix_of iristr of
		(0, _) => NONE
	      | (len, ns) => SOME (ns, String.extract (iristr, len, NONE))
	end

    fun abbreviate (table, iri) =
        (*!!! how to decide between abbreviate_reducing and
              abbreviate_matching? or some intrinsically better
              method, e.g. a trie... *)
        (*!!! + we should be storing wide strings *)
        abbreviate_matching (table, Iri.toString iri)
            
end
