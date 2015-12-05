
structure Iri :> IRI = struct

    type t = int

    structure StringMap = RedBlackMapFn (struct
                                          type ord_key = string
                                          val compare = String.compare
                                          end)

    structure IntMap = RedBlackMapFn (struct
                                       type ord_key = int
                                       val compare = Int.compare
                                       end)
                  
    val forward_map = ref StringMap.empty
    val reverse_map = ref IntMap.empty
    val next_id = ref 0

    fun fromString s =
        case StringMap.find (!forward_map, s) of
            SOME id => id
          | NONE =>
            let val id = !next_id in
                forward_map := StringMap.insert (!forward_map, s, id);
                reverse_map := IntMap.insert (!reverse_map, id, s);
                next_id := id + 1;
                id
            end

    fun toString id =
        case IntMap.find (!reverse_map, id) of
            SOME str => str
          | NONE => raise Fail ("Unknown IRI id: " ^ (Int.toString id))

    fun equals (id1, id2) =
        id1 = id2

    val compare = Int.compare

    val empty_iri = fromString ""
                      
end

                           
