
signature CODEPOINT_SET = sig
    type t

    val fromString : string -> t
    val fromRange : word -> word -> t
    val fromAsciiRange : char -> char -> t
    val fromWord : word -> t
    val union : t list -> t
    val difference : t * t -> t
    val equal : t * t -> bool

    val contains : t -> word -> bool

    val toString : t -> string
    val toText : t -> string

    val withName : string -> t -> t
    val name : t -> string
end
			   
structure CodepointSet :> CODEPOINT_SET = struct

    structure CP = RedBlackSetFn (struct
			           type ord_key = Word.word
			           val compare = Word.compare
			           end)

    type t = CP.set * string
	         
    fun ascii c =
        Word.fromInt (Char.ord c)
	             
    fun fromString str =
        (foldl CP.add' CP.empty (WdString.explodeUtf8 str), "")

    fun fromWord w =
        (CP.add (CP.empty, w), "")
	       
    fun fromRange a b =
        let fun rangeAux a b cp =
	        if a > b then cp
	        else rangeAux (a + 0w1) b (CP.add (cp, a))
        in
	    (rangeAux a b CP.empty, "")
        end

    fun fromAsciiRange start finish =
        fromRange (ascii start) (ascii finish)

    fun union (cps : t list) =
        (foldl CP.union CP.empty (map #1 cps), "")

    fun difference ((cp1, _), (cp2, _)) =
        (CP.difference (cp1, cp2), "")

    fun equal ((cp1, _), (cp2, _)) =
        CP.equal (cp1, cp2)
		 
    fun contains (cp, _) w =
        CP.member (cp, w)

    fun toString (cp, _) =
        WdString.implodeToUtf8 (CP.listItems cp)

    fun toText (cp, _) =
        String.concatWith "," (map Word.toString (CP.listItems cp))

    fun withName name (cp, _) =
        (cp, name)

    fun name (_, name) =
        name
            
end
