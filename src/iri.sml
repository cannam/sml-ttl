
structure Iri :> IRI = struct

    type t = int

    fun compare_backwards (s1, s2) =
        (* because IRIs often have common prefixes, comparing from
           their ends is often faster *)
        let val n1 = String.size s1
            val n2 = String.size s2
            fun compare' ~1 = EQUAL
              | compare' n = 
                case Char.compare (String.sub (s1, n), String.sub (s2, n)) of
                    LESS => LESS
                  | GREATER => GREATER
                  | EQUAL => compare' (n - 1)
        in
            if n1 < n2 then LESS
            else if n1 > n2 then GREATER
            else compare' (n1 - 1)
        end
                 
    structure IriMap = RedBlackMapFn (struct
                                       type ord_key = word list
                                       val compare = List.collate Word.compare
                                       end)

    val forward_map = ref IriMap.empty
    val reverse_map = IntHashTable.mkTable (2000, Fail "hash table failure")
    val next_id = ref 0

    fun fromCodepoints ww =
        case IriMap.find (!forward_map, ww) of
            SOME id => id
          | NONE =>
            let val id = !next_id in
                forward_map := IriMap.insert (!forward_map, ww, id);
                IntHashTable.insert reverse_map (id, ww);
                next_id := id + 1;
                id
            end

    fun toCodepoints id =
        case IntHashTable.find reverse_map id of
            SOME ww => ww
          | NONE => raise Fail ("Unknown IRI id: " ^ (Int.toString id))

    fun fromString s = fromCodepoints (Utf8.explodeString s)

    fun toString id = Utf8.implodeString (toCodepoints id)

    fun equals (id1, id2) = id1 = id2

    val compare = Int.compare

    val empty_iri = fromCodepoints []

    fun is_empty id = (id = empty_iri)
                      
end

                           
