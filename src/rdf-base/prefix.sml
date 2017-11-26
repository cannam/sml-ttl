		    
structure Prefix :> PREFIX where type iri = Iri.t = struct

    type iri = Iri.t
    type prefix = string * iri  (* prefix, expansion *)

    fun string_of_prefix (pre, exp) =
        "\"" ^ pre ^ "\" -> <" ^ (Iri.toString exp) ^ ">"
                                
end

