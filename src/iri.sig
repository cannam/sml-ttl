
(* Interned string, for compact storage. *)

signature IRI = sig

    type t

    val fromString : string -> t
    val toString : t -> string

    val equals : t * t -> bool
    val compare : t * t -> order

    val empty_iri : t
                               
end
                    
