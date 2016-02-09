
signature PREFIX_TABLE = sig

    type t
    type iri = Iri.t

    val empty : t
    val from_prefixes : (string * iri) list -> t (*!!! or use existing prefix type? *)
    val add : t * string * iri -> t
    val contains : t * string -> bool
    val remove : t * string -> t
    val enumerate : t -> (string * iri) list

    val expand : t * string -> iri
    val abbreviate : t * iri -> (string * string) option (* NONE if no prefix matches *)

end
