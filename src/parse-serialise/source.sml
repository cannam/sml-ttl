
signature SOURCE = sig

    type t

    val fromStream : TextIO.instream -> t
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
        stream = CodepointIO.fromTextInStream str,
        lineno = ref 0,
        colno = ref 0
    }
                  
    fun peek (r : t) : word =
        case CodepointIO.peek1 (#stream r) of
            NONE => nl
          | SOME w => w

    fun peekN n (r : t) : word list =
        WdString.explode (CodepointIO.peekN (#stream r, n))

    fun read (r : t) : word =
        case CodepointIO.input1 (#stream r) of
            NONE => nl
          | SOME w => w

    fun readN n (r : t) : word list =
        WdString.explode (CodepointIO.inputN (#stream r, n))

    fun discard r = ignore (read r)

    fun location (r : t) = "(unknown)" (*!!!*)
(*        "line " ^ (Int.toString (!(#lineno r))) ^
        ", column " ^ (Int.toString (!(#colno r) + 1))
*)
    fun eof (r : t) =
        CodepointIO.endOfStream (#stream r)
	    
end
