
structure StoreProperty :> STORE_PROPERTY = struct

    type iri = Iri.t
    datatype node = datatype RdfNode.node
    type store = Store.t

    open RdfNode

    fun match (s : Store.t, subject, name) = 
        Store.match (s, (SOME subject,
                         SOME (IRI (Store.expand (s, name))),
                         NONE))
                   
    fun textList args =
        List.mapPartial (fn (_, _, LITERAL { value, ... }) => SOME value
                        | _ => NONE)
                        (match args)

    fun text args =
        case textList args of
            result::_ => result
          | [] => ""

    fun iriList args = 
        List.mapPartial (fn (_, _, IRI iri) => SOME iri
                        | _ => NONE)
                        (match args)

    fun iri args =
        case iriList args of
            result::_ => SOME result
          | [] => NONE
                      
    fun nodeList args =
        List.mapPartial (fn (_, _, IRI iri) => SOME (IRI iri)
                        | (_, _, BLANK b) => SOME (BLANK b)
                        | _ => NONE)
                        (match args)

    fun node args =
        case nodeList args of
            result::_ => SOME result
          | [] => NONE
                     
end


