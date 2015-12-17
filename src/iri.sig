
(* Interned string, for compact storage. *)

signature IRI = sig

    type t

    val fromString : string -> t
    val toString : t -> string

    val fromWideString : SimpleWideString.t -> t
    val toWideString : t -> SimpleWideString.t
                            
    val equals : t * t -> bool
    val compare : t * t -> order  (* _not_ lexicographic comparison *)

    val empty_iri : t
    val is_empty : t -> bool
                               
end
                    
