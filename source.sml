
signature SOURCE = sig

    type t

    val from_stream : TextIO.instream -> t
    val peek : t -> word
    val read : t -> word
    val discard : t -> t
    val location : t -> int * int
    val location_string : t -> string
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

    fun peek r =
        case !(#line r) of
            first::rest => first
          | [] => nl

    fun read r =
        case !(#line r) of
            first::next::rest =>
            ((#line r) := next::rest;
             (#colno r) := !(#colno r) + 1;
             first)
          | first::[] => (load_line r; first)
          | [] => nl

    fun discard r =
        let val _ = read r in r end
                      
    fun location r =
        (!(#lineno r), !(#colno r))

    fun location_string r =
        "line " ^ (Int.toString (!(#lineno r))) ^
        ", column " ^ (Int.toString (!(#colno r)))

    fun eof r =
        (!(#line r) = [])
end
