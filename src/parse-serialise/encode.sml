signature ENCODE = sig

    val uEncode : word -> word list
    val percentEncode : word -> word list
    val percentOrUEncode : word -> word list
    val asciiEncode : word -> word list
    val backslashEncode : word -> word list

    type encoder = (CodepointSet.t * (word -> word list))
                                    
    val encodeWdstring : encoder -> WdString.t -> string
    val encodeWdstringExcept : encoder -> WdString.t -> string

    val encodeString : encoder -> string -> string
    val encodeStringExcept : encoder -> string -> string
                                   
end

structure Encode :> ENCODE = struct

    type encoder = (CodepointSet.t * (word -> word list))
                                    
    fun hexStringOf (w, min) =
        let fun padded h =
                let val len = String.size h in
                    if len < min then padded ("0" ^ h)
                    else if Int.mod (String.size h, 2) = 1 then "0" ^ h
                    else h
                end
        in
            padded (Word.toString w)
        end
                                   
    fun uEncode w =
        WdString.explodeUtf8
            (if w > 0wxffff
             then "\\U" ^ (hexStringOf (w, 8))
             else "\\u" ^ (hexStringOf (w, 4)))
                                        
    fun percentEncode w = WdString.explodeUtf8 ("%" ^ (hexStringOf (w, 0)))

    fun percentOrUEncode w =
        if w > 0wx00ff then uEncode w
        else percentEncode w
                                              
    fun asciiEncode 0wx09 = WdString.explodeUtf8 "\\t"
      | asciiEncode 0wx0A = WdString.explodeUtf8 "\\n"
      | asciiEncode 0wx0D = WdString.explodeUtf8 "\\r"
      | asciiEncode 0wx22 = WdString.explodeUtf8 "\\\""
      | asciiEncode 0wx5C = WdString.explodeUtf8 "\\\\"
      | asciiEncode w = uEncode w

    fun backslashEncode w =
        [Word.fromInt (Char.ord #"\\"), w]
                                  
    fun encodeW tester (cps, encodeFn) token =
        let fun encode (w, acc) =
                if tester (CodepointSet.contains cps w)
                then w :: acc
                else (encodeFn w) @ acc
        in
            WdString.implodeToUtf8
                (WdString.foldr encode [] token)
        end

    val encodeWdstring = encodeW (fn x => x)

    val encodeWdstringExcept = encodeW not

    fun encodeString encoder str =
        encodeWdstring encoder (WdString.fromUtf8 str)

    fun encodeStringExcept encoder str =
        encodeWdstringExcept encoder (WdString.fromUtf8 str)

end
                       
