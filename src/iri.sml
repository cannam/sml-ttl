
structure Iri :> IRI = struct

    type t = int

    (* Because IRIs often have common prefixes, comparing from their
       ends is often faster. We use this helper for prefix checking
       and for the intern map compare function; the IRI equal and
       compare functions just use intern ids. *)
    fun compare_prefix_backwards (_, _, 0) = EQUAL
      | compare_prefix_backwards (s1, s2, n) =
	let val m = n-1
	in
            case Word.compare (WdString.sub (s1, m),
                               WdString.sub (s2, m)) of
		LESS => LESS
              | GREATER => GREATER
              | EQUAL => compare_prefix_backwards (s1, s2, m)
	end

    fun compare_backwards (s1, s2) =
        let val n1 = WdString.size s1
            val n2 = WdString.size s2
        in
            if n1 < n2 then LESS
            else if n1 > n2 then GREATER
            else compare_prefix_backwards (s1, s2, n1)
        end

    fun is_prefix (s1, s2) =
        let val n1 = WdString.size s1
            val n2 = WdString.size s2
        in
	    if n1 > n2 then false
	    else compare_prefix_backwards (s1, s2, n1) = EQUAL
	end
	    
    structure IriMap = RedBlackMapFn (struct
                                       type ord_key = WdString.t
                                       val compare = compare_backwards
                                       end)

    val forward_map : int IriMap.map ref =
        ref IriMap.empty
	
    val reverse_map : WdString.t IntHashTable.hash_table =
        IntHashTable.mkTable (2000, Fail "hash table failure")

    val next_id : int ref = ref 0

    fun fromWideString ww =
        case IriMap.find (!forward_map, ww) of
            SOME id => id
          | NONE =>
            let val id = !next_id in
                forward_map := IriMap.insert (!forward_map, ww, id);
                IntHashTable.insert reverse_map (id, ww);
                next_id := id + 1;
                id
            end

    fun toWideString id =
        case IntHashTable.find reverse_map id of
            SOME ww => ww
          | NONE => raise Fail ("Unknown IRI id: " ^ (Int.toString id))

    fun fromString s = fromWideString (WdString.fromUtf8 s)

    fun toString id = WdString.toUtf8 (toWideString id)

    fun equals (id1, id2) = id1 = id2

    val compare = Int.compare

    fun isPrefixOf (s, iri) = is_prefix (s, toWideString iri) 
		      
    val empty = fromString ""

    fun isEmpty id = (id = empty)
                      
end

                           
