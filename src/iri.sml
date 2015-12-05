
structure Iri :> IRI = struct

    type t = int

    structure StringHash = HashTableFn (HashString)

    val forward_map = StringHash.mkTable (500, Fail "hash table failure") (*!!!*)
    val reverse_map = IntHashTable.mkTable (500, Fail "hash table failure") (*!!!*)
    val next_id = ref 0

    fun fromString s =
        case StringHash.find forward_map s of
            SOME id => id
          | NONE =>
            let val id = !next_id in
                StringHash.insert forward_map (s, id);
                IntHashTable.insert reverse_map (id, s);
                next_id := id + 1;
                id
            end

    fun toString id =
        case IntHashTable.find reverse_map id of
            SOME str => str
          | NONE => raise Fail ("Unknown IRI id: " ^ (Int.toString id))

    fun equals (id1, id2) =
        id1 = id2

    val compare = Int.compare

    val empty_iri = fromString ""
                      
end

                           
