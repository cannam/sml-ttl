
structure Iri :> IRI = struct

    type t = int

    structure Comp = StringCompare(struct
                                    type str = WdString.t
                                    type ch = word
                                    val size = WdString.size
                                    val sub = WdString.sub
                                    val ch_compare = Word.compare
                                    end)
                 
    structure IriMap = RedBlackMapFn (struct
                                       type ord_key = WdString.t
                                       val compare = Comp.compare_backwards
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

    val compare = Int.compare

    val empty = fromString ""

    fun isEmpty id = (id = empty)

    fun addSuffix (iri, suffix) =
        fromWideString (WdString.concatWith WdString.empty
                                            [toWideString iri, suffix])
                         
end

                           
