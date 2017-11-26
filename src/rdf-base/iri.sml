
structure Iri :> IRI = struct

    type t = int

    structure Comp = StringCompare(struct
                                    type str = WdString.t
                                    type ch = word
                                    val size = WdString.size
                                    val sub = WdString.sub
                                    val chCompare = Word.compare
                                    end)
                 
    structure IriMap = RedBlackMapFn (struct
                                       type ord_key = WdString.t
                                       val compare = Comp.compareBackwards
                                       end)

    val forwardMap : int IriMap.map ref =
        ref IriMap.empty
	
    val reverseMap : WdString.t IntHashTable.hash_table =
        IntHashTable.mkTable (2000, Fail "hash table failure")

    val nextId : int ref = ref 0

    fun fromWideString ww =
        case IriMap.find (!forwardMap, ww) of
            SOME id => id
          | NONE =>
            let val id = !nextId in
                forwardMap := IriMap.insert (!forwardMap, ww, id);
                IntHashTable.insert reverseMap (id, ww);
                nextId := id + 1;
                id
            end

    fun toWideString id =
        case IntHashTable.find reverseMap id of
            SOME ww => ww
          | NONE => raise Fail ("Unknown IRI id: " ^ (Int.toString id))

    fun fromString s = fromWideString (WdString.fromUtf8 s)

    fun toString id = WdString.toUtf8 (toWideString id)

    val compare = Int.compare

    val empty = fromString ""

    fun isEmpty id = (id = empty)

    fun size iri = WdString.size (toWideString iri)
			 
    fun addSuffix (iri, suffix) =
        fromWideString (WdString.concatWith WdString.empty
                                            [toWideString iri, suffix])
                         
end

                           
