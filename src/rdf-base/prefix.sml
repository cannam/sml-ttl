		    
structure Prefix :> PREFIX where type iri = Iri.t = struct

    type iri = Iri.t
    type abbreviation = string
    type prefix = abbreviation * iri
    type curie = string

    fun stringOfPrefix (pre, exp) =
        "\"" ^ pre ^ "\" -> <" ^ (Iri.toString exp) ^ ">"
                                
end

