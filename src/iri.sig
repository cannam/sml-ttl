
(* Interned string, for compact storage. *)

signature IRI = sig

    type t

    val fromString : string -> t
    val toString : t -> string

    val fromCodepoints : Utf8.t -> t
    val toCodepoints : t -> Utf8.t
                            
    val equals : t * t -> bool
    val compare : t * t -> order  (* _not_ lexicographic comparison *)

    val empty_iri : t
    val is_empty : t -> bool
                               
end
                    
