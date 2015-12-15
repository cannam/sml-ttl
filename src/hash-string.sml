
structure HashString : HASH_KEY = struct

    fun hashChar (c, h) = 0w5 * h + Word.fromInt (Char.ord c)
    fun hash (s, ix, len) =
        List.foldr hashChar 0w0 (List.take (List.drop (String.explode s, ix), len))

    fun hashString s = hash (s, 0, size s)
    fun hashSubstring ss = hash (Substring.base ss)

    type hash_key = string
                                
    val hashVal = hashString
    fun sameKey (k1, k2) = k1 = k2
                                
end
