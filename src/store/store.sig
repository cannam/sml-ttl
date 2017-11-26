		      
signature STORE = sig

    include MATCHER

    type iri = Iri.t
    type prefix = Prefix.prefix
    type abbreviation = Prefix.abbreviation
    type curie = string

    val empty : t
    val add : t * triple -> t
    val contains : t * triple -> bool
    val remove : t * triple -> t
    val foldl_match : (triple * 'a -> 'a) -> 'a -> (t * pattern) -> 'a
    val foldl : (triple * 'a -> 'a) -> 'a -> t -> 'a
    val enumerate : t -> triple list
    (* + match from MATCHER *)

    val add_prefix : t * prefix -> t
    val contains_prefix : t * abbreviation -> bool
    val remove_prefix : t * abbreviation -> t
    val enumerate_prefixes : t -> prefix list
    val expand : t * curie -> iri
    val abbreviate : t * iri -> (abbreviation * string) option (* NONE if no prefix matches *)
    val get_prefix_table : t -> PrefixTable.t
	     
end

