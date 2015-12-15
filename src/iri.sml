
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
                 
    structure StringMap = RedBlackMapFn (struct
                                          type ord_key = string
                                          val compare = compare_backwards
                                          end)

    val forward_map = ref StringMap.empty
    val reverse_map = IntHashTable.mkTable (2000, Fail "hash table failure")
    val next_id = ref 0

    fun fromString s =
        case StringMap.find (!forward_map, s) of
            SOME id => id
          | NONE =>
            let val id = !next_id in
                forward_map := StringMap.insert (!forward_map, s, id);
                IntHashTable.insert reverse_map (id, s);
                next_id := id + 1;
                id
            end

    fun toString id =
        case IntHashTable.find reverse_map id of
            SOME str => str
          | NONE => raise Fail ("Unknown IRI id: " ^ (Int.toString id))

    fun equals (id1, id2) = id1 = id2

    val compare = Int.compare

    val empty_iri = fromString ""

    fun is_empty id = (id = empty_iri)
                      
end

                           
