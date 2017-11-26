		      
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
    val foldlMatch : (triple * 'a -> 'a) -> 'a -> (t * pattern) -> 'a
    val foldl : (triple * 'a -> 'a) -> 'a -> t -> 'a
    val enumerate : t -> triple list
    (* + match from MATCHER *)

    val addPrefix : t * prefix -> t
    val containsPrefix : t * abbreviation -> bool
    val removePrefix : t * abbreviation -> t
    val enumeratePrefixes : t -> prefix list
    val expand : t * curie -> iri
    val abbreviate : t * iri -> (abbreviation * string) option (* NONE if no prefix matches *)
    val getPrefixTable : t -> PrefixTable.t
	     
end

