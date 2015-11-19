
signature CODEPOINTS = sig
    type t

    val from_string : string -> t
    val from_range : word -> word -> t
    val from_ascii_range : char -> char -> t
    val from_word : word -> t

    val union : t list -> t

    val contains : t -> word -> bool
end
			   
structure Codepoints :> CODEPOINTS = struct

structure CP = SplaySetFn (struct
			    type ord_key = Word.word
			    val compare = Word.compare
			    end)

type t = CP.set
	  
fun ascii c =
    Word.fromInt (Char.ord c)
	      
fun from_string str =
    foldl CP.add' CP.empty (Utf8.explode (Utf8.fromString str))

fun from_word w =
    CP.add (CP.empty, w)
	  
fun from_range a b =
    let fun range_aux a b cp =
	    if a > b then cp
	    else range_aux (a + 0w1) b (CP.add (cp, a))
    in
	range_aux a b CP.empty
    end

fun from_ascii_range start finish =
    from_range (ascii start) (ascii finish)

fun union cps =
    foldl CP.union CP.empty cps

fun contains cp w =
    CP.member (cp, w)

end
