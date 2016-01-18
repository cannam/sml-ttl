
signature STRING_COMPARE_ARG = sig

    type str
    type ch
             
    val size : str -> int
    val sub : str * int -> ch
    val ch_compare : ch * ch -> order 
    
end

signature STRING_COMPARE = sig

    type str
             
    val compare_backwards : str * str -> order
    val equals : str * str -> bool
    val is_prefix : str * str -> bool
    
end
                                 
(* Because IRIs often have common prefixes, comparing them in reverse
   is usually faster. The same applies to prefix comparison --
   prefixes often have common prefixes of their own. *)

functor StringCompare (S : STRING_COMPARE_ARG) : STRING_COMPARE = struct

    type str = S.str
    
    fun compare_prefix_backwards (s1, s2, 0) = EQUAL
      | compare_prefix_backwards (s1, s2, plen) =
        let val m = plen - 1
        in
            case S.ch_compare (S.sub (s1, m),
                               S.sub (s2, m)) of
                LESS => LESS
              | GREATER => GREATER
              | EQUAL => compare_prefix_backwards (s1, s2, m)
        end

    fun compare_backwards (s1, s2) =
        let val n1 = S.size s1
            val n2 = S.size s2
        in
            if n1 < n2 then LESS
            else if n1 > n2 then GREATER
            else compare_prefix_backwards (s1, s2, n1)
        end

    fun equals (s1, s2) =
        let val n1 = S.size s1
            val n2 = S.size s2
        in
            if n1 <> n2 then false
            else compare_prefix_backwards (s1, s2, n1) = EQUAL
        end
            
    fun is_prefix (s1, s2) =
        let val n1 = S.size s1
            val n2 = S.size s2
        in
            if n1 > n2 then false
            else compare_prefix_backwards (s1, s2, n1) = EQUAL
        end

end
                                
