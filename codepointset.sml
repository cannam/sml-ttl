
signature CODEPOINT_SET = sig
    type t

    val from_string : string -> t
    val from_range : word -> word -> t
    val from_ascii_range : char -> char -> t
    val from_word : word -> t
    val union : t list -> t
    val difference : t * t -> t
    val equal : t * t -> bool

    val contains : t -> word -> bool

    val to_string : t -> string
    val to_text : t -> string
end
			   
structure CodepointSet :> CODEPOINT_SET = struct

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

fun difference (cp1, cp2) =
    CP.difference (cp1, cp2)

fun equal (cp1, cp2) =
    CP.equal (cp1, cp2)
		  
fun contains cp w =
    CP.member (cp, w)

fun to_string cp =
    Utf8Encode.encode_string (CP.listItems cp)

fun to_text cp =
    String.concatWith "," (map Word.toString (CP.listItems cp))
                             
end