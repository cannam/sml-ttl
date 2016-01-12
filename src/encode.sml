signature ENCODE = sig

    val u_encode : word -> word list
    val percent_encode : word -> word list
    val percent_or_u_encode : word -> word list
    val ascii_encode : word -> word list
    val backslash_encode : word -> word list

    type encoder = (CodepointSet.t * (word -> word list))
                                    
    val encode_wdstring : encoder -> WdString.t -> string
    val encode_wdstring_except : encoder -> WdString.t -> string

    val encode_string : encoder -> string -> string
    val encode_string_except : encoder -> string -> string
                                   
end

structure Encode :> ENCODE = struct

    type encoder = (CodepointSet.t * (word -> word list))
                                    
    fun hex_string_of (w, min) =
        let fun padded h =
                let val len = String.size h in
                    if len < min then padded ("0" ^ h)
                    else if Int.mod (String.size h, 2) = 1 then "0" ^ h
                    else h
                end
        in
            padded (Word.toString w)
        end
                                   
    fun u_encode w =
        WdString.explodeUtf8
            (if w > 0wxffff
             then "\\U" ^ (hex_string_of (w, 8))
             else "\\u" ^ (hex_string_of (w, 4)))
                                        
    fun percent_encode w = WdString.explodeUtf8 ("%" ^ (hex_string_of (w, 0)))

    fun percent_or_u_encode w =
        if w > 0wx00ff then u_encode w
        else percent_encode w
                                              
    fun ascii_encode 0wx09 = WdString.explodeUtf8 "\\t"
      | ascii_encode 0wx0A = WdString.explodeUtf8 "\\n"
      | ascii_encode 0wx0D = WdString.explodeUtf8 "\\r"
      | ascii_encode 0wx22 = WdString.explodeUtf8 "\\\""
      | ascii_encode 0wx5C = WdString.explodeUtf8 "\\\\"
      | ascii_encode w = u_encode w

    fun backslash_encode w =
        [Word.fromInt (Char.ord #"\\"), w]
                                  
    fun encode_w tester (cps, encode_fn) token =
        let fun encode (w, acc) =
                if tester (CodepointSet.contains cps w)
                then w :: acc
                else (encode_fn w) @ acc
        in
            WdString.implodeToUtf8
                (WdString.foldr encode [] token)
        end

    val encode_wdstring = encode_w (fn x => x)

    val encode_wdstring_except = encode_w not

    fun encode_string encoder str =
        encode_wdstring encoder (WdString.fromUtf8 str)

    fun encode_string_except encoder str =
        encode_wdstring_except encoder (WdString.fromUtf8 str)

end
                       
