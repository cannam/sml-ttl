
(* should be "store property" like "store collection"? *)

structure Property : PROPERTY = struct

    datatype node = datatype RdfNode.node
    type iri = Iri.t

    fun match (s : Store.t, subject, name) = 
        let open Store
        in match (s, (KNOWN subject,
                      KNOWN (IRI (expand (s, name))),
                      WILDCARD))
        end
                   
    fun text_list args =
        List.mapPartial (fn (_, _, LITERAL { value, ... }) => SOME value
                        | _ => NONE)
                        (match args)

    fun text args =
        case text_list args of
            result::_ => result
          | [] => ""

    fun iri_list args = 
        List.mapPartial (fn (_, _, IRI iri) => SOME iri
                        | _ => NONE)
                        (match args)

    fun iri args =
        case iri_list args of
            result::_ => SOME result
          | [] => NONE
                      
    fun node_list args =
        List.mapPartial (fn (_, _, IRI iri) => SOME (IRI iri)
                        | (_, _, BLANK b) => SOME (BLANK b)
                        | _ => NONE)
                        (match args)

    fun node args =
        case node_list args of
            result::_ => SOME result
          | [] => NONE
                     
end


