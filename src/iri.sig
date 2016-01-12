
(* Interned string, for compact storage. *)

signature IRI = sig

    eqtype t

    val fromString : string -> t
    val toString : t -> string

    val fromWideString : WdString.t -> t
    val toWideString : t -> WdString.t

    (* The comparison functions are *not* lexicographic, they just produce some
       consistent ordering as quickly as possible *)
    val equals : t * t -> bool
    val compare : t * t -> order

    val isPrefixOf : WdString.t * t -> bool (* prefix, iri *)

    val empty : t
    val isEmpty : t -> bool
                               
end
                    
