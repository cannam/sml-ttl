
(* Interned string, for compact storage. *)

signature IRI = sig

    eqtype t

    val fromString : string -> t
    val toString : t -> string

    val fromWideString : WdString.t -> t
    val toWideString : t -> WdString.t

    (* The comparison function is *not* lexicographic, it just produces some
       consistent ordering as quickly as possible *)
    val compare : t * t -> order

    val empty : t
    val isEmpty : t -> bool

    (* Return the length of the IRI in ISO 10646-1 codepoints *)
    val size : t -> int

    val addSuffix : (t * WdString.t) -> t
                            
end
                    
