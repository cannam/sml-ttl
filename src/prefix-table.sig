
signature PREFIX_TABLE = sig

    type t
    type iri = Iri.t

    val empty : t
    val from_prefixes : (string * string) list -> t
    val add : t * string * string -> t
    val contains : t * string -> bool
    val remove : t * string -> t
    val enumerate : t -> (string * string) list

    val expand : t * string -> iri
    val abbreviate : t * iri -> (string * string) option (* NONE if no prefix matches *)

end
