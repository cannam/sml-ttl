
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
        stream : TextIO.instream,
        line : WdString.t ref,
        lineno : int ref,
        colno : int ref
    }

    fun loadLine (r : t) =
        (case TextIO.inputLine (#stream r) of
             NONE =>
             (#line r) := WdString.empty
           | SOME str =>
             ((#line r) := WdString.fromUtf8 str;
              (#lineno r) := !(#lineno r) + 1;
              (#colno r) := 0);
         r)

    fun fromStream str =
        loadLine {
            stream = str,
            line = ref WdString.empty,
            lineno = ref 0,
            colno = ref 0
        }

    fun inRange (r : t) =
        !(#colno r) < WdString.size (!(#line r))
                  
    fun peek (r : t) =
        if inRange r
        then WdString.sub (!(#line r), !(#colno r))
        else nl

    fun peekN n (r : t) =
        let val line = !(#line r)
            val len = WdString.size line
            fun peek' 0 c = []
              | peek' n c = 
                if c < len
                then WdString.sub (line, c) :: peek' (n-1) (c+1)
                else []
        in
            peek' n (!(#colno r))
        end

    fun read (r : t) =
        if inRange r
        then let val w = WdString.sub (!(#line r), !(#colno r))
             in
                 ((#colno r) := !(#colno r) + 1;
                  if not (inRange r) then (loadLine r; w)
                  else w)
             end
        else nl

    fun readN 0 (r : t) = []
      | readN n (r : t) =
         read r :: (readN (n-1) r)

    fun discard r = ignore (read r)

    fun location (r : t) =
        "line " ^ (Int.toString (!(#lineno r))) ^
        ", column " ^ (Int.toString (!(#colno r) + 1))

    fun eof (r : t) = not (inRange r)
	    
end
