
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
        line : Utf8.t ref,
        lineno : int ref,
        colno : int ref
    }

    fun load_line r =
        (case TextIO.inputLine (#stream r) of
             NONE =>
             (#line r) := Utf8.empty
           | SOME str =>
             ((#line r) := Utf8.fromString str;
              (#lineno r) := !(#lineno r) + 1;
              (#colno r) := 0);
         r)

    fun from_stream str =
        load_line { stream = str, line = ref Utf8.empty, lineno = ref 0, colno = ref 0 }

    fun in_range (r : t) =
        !(#colno r) < Utf8.size (!(#line r))
                  
    fun peek (r : t) =
        if in_range r
        then Utf8.sub (!(#line r), !(#colno r))
        else nl

    fun peek_n n (r : t) =
        let val line = !(#line r)
            val len = Utf8.size line
            fun peek' 0 c = []
              | peek' n c = 
                (if c < len
                 then Utf8.sub (line, c)
                 else nl) :: (peek' (n-1) (c+1))
        in
            peek' n (!(#colno r))
        end

    fun read (r : t) =
        if in_range r
        then let val w = Utf8.sub (!(#line r), !(#colno r))
             in
                 ((#colno r) := !(#colno r) + 1;
                  if not (in_range r) then (load_line r; w)
                  else w)
             end
        else nl

    fun read_n 0 (r : t) = []
      | read_n n (r : t) =
         read r :: (read_n (n-1) r)

    fun discard r = ignore (read r)

    fun location (r : t) =
        "line " ^ (Int.toString (!(#lineno r))) ^
        ", column " ^ (Int.toString (!(#colno r) + 1))

    fun eof (r : t) = not (in_range r)
	    
end
