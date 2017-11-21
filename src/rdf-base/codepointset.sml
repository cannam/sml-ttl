
signature CODEPOINT_SET = sig
    type t

    val from_string : string -> t
    val from_range : word -> word -> t
    val from_ascii_range : char -> char -> t
    val from_word : word -> t
    val union : t list -> t
    val equal : t * t -> bool

    val contains : t -> word -> bool

    val to_string : t -> string
    val to_text : t -> string

    val with_name : string -> t -> t
    val name : t -> string
end
			   
structure CodepointSet :> CODEPOINT_SET = struct

(*!!! if this is going to have its own custom implementation, it's going to need its own custom tests *)

    type t = word vector * string

    fun condense ww =
        Vector.fromList (ListMergeSort.sort Word.> ww)
          
    fun plode v =
        rev (Vector.foldl (op::) [] v)

    fun find (v, w) = (* binary search for w in v *)
        let fun find' (a, b) =
                if a >= b then false
                else
                    let val mid = a + ((b - a) div 2)
                    in
                        case Word.compare (w, Vector.sub (v, mid)) of
                            EQUAL => true
                          | LESS => find' (a, mid)
                          | GREATER => find' (mid + 1, b)
                    end
            val len = Vector.length v
        in
            if len = 0 orelse
               Word.< (w, Vector.sub (v, 0)) orelse
               Word.> (w, Vector.sub (v, len - 1))
            then false
            else find' (0, len)
        end 
            
    fun ascii c =
        Word.fromInt (Char.ord c)
	             
    fun from_string str =
        (condense (WdString.explodeUtf8 str), "")

    fun from_word w =
        (Vector.fromList [w], "")
	       
    fun from_range a b =
        let fun range_aux a b acc =
	        if a > b then Vector.fromList (rev acc)
	        else range_aux (a + 0w1) b (a :: acc)
        in
	    (range_aux a b [], "")
        end

    fun from_ascii_range start finish =
        from_range (ascii start) (ascii finish)

    fun union (cps : t list) =
        (condense (List.concat (map (fn (v, _) => plode v) cps)), "")

    fun equal ((cp1, _), (cp2, _)) =
        Vector.collate Word.compare (cp1, cp2) = EQUAL
		 
    fun contains (cp, _) w =
        find (cp, w)

    fun to_string (cp, _) =
        WdString.implodeToUtf8 (plode cp)

    fun to_text (cp, _) =
        String.concatWith "," (map Word.toString (plode cp))

    fun with_name name (cp, _) =
        (cp, name)

    fun name (_, name) =
        name
            
end
