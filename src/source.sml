
signature SOURCE = sig

    type t

    val from_stream : TextIO.instream -> t
    val peek : t -> word
    val peek_n : int -> t -> word list
    val read : t -> word
    val read_n : int -> t -> word list
    val discard : t -> unit
    val location : t -> string
    val eof : t -> bool

end
                         
structure Source :> SOURCE = struct

    val nl = Word.fromInt (Char.ord #"\n")

    type t = {
        stream : TextIO.instream,
        line : word list ref,
        lineno : int ref,
        colno : int ref
    }

    fun load_line r =
        (case TextIO.inputLine (#stream r) of
             NONE =>
             (#line r) := []
           | SOME str =>
             ((#line r) := Utf8.explode (Utf8.fromString str);
              (#lineno r) := !(#lineno r) + 1;
              (#colno r) := 1);
         r)

    fun from_stream str =
        load_line { stream = str, line = ref [], lineno = ref 0, colno = ref 0 }

    fun peek (r : t) =
        case !(#line r) of
            first::rest => first
          | [] => nl

    fun peek_n n (r : t) =
        List.take (!(#line r), n)
        handle Subscript => []

    fun read (r : t) =
        case !(#line r) of
            first::next::rest =>
            ((#line r) := next::rest;
             (#colno r) := !(#colno r) + 1;
             first)
          | first::[] => (load_line r; first)
          | [] => nl

    fun read_n 0 (r : t) = []
      | read_n n (r : t) =
         read r :: (read_n (n-1) r)

    fun discard r =
        let val _ = read r in () end

    fun location (r : t) =
        "line " ^ (Int.toString (!(#lineno r))) ^
        ", column " ^ (Int.toString (!(#colno r)))

    fun eof (r : t) =
	(!(#line r) = [])
	    
end
