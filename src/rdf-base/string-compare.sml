
signature STRING_COMPARE_ARG = sig

    type str
    type ch
             
    val size : str -> int
    val sub : str * int -> ch
    val chCompare : ch * ch -> order 
    
end


(* Because IRIs often have common prefixes, comparing them in reverse
   is usually faster. The same applies to prefix comparison --
   prefixes often have common prefixes of their own. This signature
   specifies a reversed string comparator with prefix testing. *)

signature STRING_COMPARE = sig

    type str
             
    (* Compare two strings. Shorter strings sort before longer
       strings; for strings of the same length, comparison is made in
       lexicographic order of the reversed string. *)
    val compareBackwards : str * str -> order
             
    (* Determine whether two strings are equal. *)
    val equals : str * str -> bool

    (* Determine whether the first string is a prefix of the second one. *)
    val isPrefix : str * str -> bool
    
end
                                 

functor StringCompare (S : STRING_COMPARE_ARG) : STRING_COMPARE = struct

    type str = S.str
    
    fun comparePrefixBackwards (s1, s2, 0) = EQUAL
      | comparePrefixBackwards (s1, s2, plen) =
        let val m = plen - 1
        in
            case S.chCompare (S.sub (s1, m),
                               S.sub (s2, m)) of
                LESS => LESS
              | GREATER => GREATER
              | EQUAL => comparePrefixBackwards (s1, s2, m)
        end

    fun compareBackwards (s1, s2) =
        let val n1 = S.size s1
            val n2 = S.size s2
        in
            if n1 < n2 then LESS
            else if n1 > n2 then GREATER
            else comparePrefixBackwards (s1, s2, n1)
        end

    fun equals (s1, s2) =
        let val n1 = S.size s1
            val n2 = S.size s2
        in
            if n1 <> n2 then false
            else comparePrefixBackwards (s1, s2, n1) = EQUAL
        end
            
    fun isPrefix (s1, s2) =
        let val n1 = S.size s1
            val n2 = S.size s2
        in
            if n1 > n2 then false
            else comparePrefixBackwards (s1, s2, n1) = EQUAL
        end

end
                                
