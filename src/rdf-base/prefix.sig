		    
signature PREFIX = sig

    type iri

    type prefix = string * iri  (* prefix, expansion *)

    (* Debug streaming, not guaranteed to match a standard format *)
    val string_of_prefix : prefix -> string

end

