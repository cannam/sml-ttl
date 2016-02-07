
(* should be "store property" like "store collection"? *)

structure Property : PROPERTY = struct

    datatype node = datatype RdfNode.node
    type iri = Iri.t

    fun match (s : Store.t, subject, name) = 
        Store.match (s, (SOME subject,
                         SOME (IRI (Store.expand (s, name))),
                         NONE))
                   
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


