
signature SOURCE = sig

    type t

    val fromStream : CodepointIO.instream -> t
    val peek : t -> word
    val peekN : int -> t -> word list
    val read : t -> word
    val readN : int -> t -> word list
    val discard : t -> unit
    val location : t -> string
    val eof : t -> bool

end

structure Source :> SOURCE = struct

    val nl = Word.fromInt (Char.ord #"\n")

    type t = {
        stream : CodepointIO.instream,
        lineno : int ref,
        colno : int ref
    }

    fun fromStream str = {
        stream = str,
        lineno = ref 1,
        colno = ref 0
    }

    fun advance (r : t) w =
        if w = nl
        then (#lineno r := !(#lineno r) + 1; #colno r := 0)
        else #colno r := !(#colno r) + 1

    fun advanceWith (r : t, w) =
        (advance r w; w)

    fun advanceWithList (r : t, wl) =
        (app (advance r) wl; wl)
                             
    fun peek (r : t) : word =
        case CodepointIO.peek1 (#stream r) of
            NONE => nl
          | SOME w => w

    fun peekN n (r : t) : word list =
        WdString.explode (CodepointIO.peekN (#stream r, n))

    fun read (r : t) : word =
        case CodepointIO.input1 (#stream r) of
            NONE => nl
          | SOME w => advanceWith (r, w)

    fun readN n (r : t) : word list =
        let val wl = WdString.explode (CodepointIO.inputN (#stream r, n))
        in
            advanceWithList (r, wl)
        end

    fun discard r =
        ignore (read r)

    fun location (r : t) =
        "line " ^ (Int.toString (!(#lineno r))) ^
        ", column " ^ (Int.toString (!(#colno r)))

    fun eof (r : t) =
        CodepointIO.endOfStream (#stream r)
	    
end
