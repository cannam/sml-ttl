
signature PREFIX_TABLE = sig

    type t
    type iri = Iri.t
    type prefix = Prefix.prefix
    type abbreviation = Prefix.abbreviation
    type curie = Prefix.curie
                   
    val empty : t
    val from_prefixes : prefix list -> t
    val add : t * prefix -> t
    val contains : t * abbreviation -> bool
    val remove : t * abbreviation -> t
    val enumerate : t -> prefix list

    val expand : t * curie -> iri
    val abbreviate : t * iri -> (abbreviation * string) option (* NONE if no prefix matches *)

end
