signature UTF8_ENCODE = sig
    val encode_codepoint : word -> string
    val encode_string : word list -> string
end

structure Utf8Encode :> UTF8_ENCODE = struct

fun encode_codepoint cp =
    let open Word
	fun char_of w = Char.chr (Word.toInt w)
    in
	if cp < 0wx80 then
	    String.str (char_of cp)
	else if cp < 0wx800 then
	    String.implode [
		char_of (orb (0wxc0, >> (cp, 0w6))),
		char_of (orb (0wx80, andb (cp, 0wx3f)))
	    ]
	else String.implode [
		char_of (orb (0wxe0, >> (cp, 0w12))),
		char_of (orb (0wx80, andb (>> (cp, 0w6), 0wx3f))),
		char_of (orb (0wx80, andb (cp, 0wx3f)))
	    ]
    end

fun encode_string cps =
    String.concat (map encode_codepoint cps)

end
					  
