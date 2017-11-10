		      
signature STORE = sig

    include MATCHER

    type iri = Iri.t

    val empty : t
    val add : t * triple -> t
    val contains : t * triple -> bool
    val remove : t * triple -> t
    val foldl_match : (triple * 'a -> 'a) -> 'a -> (t * pattern) -> 'a
    val foldl : (triple * 'a -> 'a) -> 'a -> t -> 'a
    val enumerate : t -> triple list
    (* + match from MATCHER *)

    val add_prefix : t * string * iri -> t (*!!! or use existing prefix type? *)
    val contains_prefix : t * string -> bool
    val remove_prefix : t * string -> t
    val enumerate_prefixes : t -> (string * iri) list
    val expand : t * string -> iri
    val abbreviate : t * iri -> (string * string) option (* NONE if no prefix matches *)
    val get_prefix_table : t -> PrefixTable.t
	     
end

