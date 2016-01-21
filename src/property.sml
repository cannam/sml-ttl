
structure Property : PROPERTY = struct

    structure Store = Store 
    datatype node = datatype RdfNode.node
    type iri = Iri.t

    fun match (store, subject, name) = 
        let open Store
        in match (store, (KNOWN subject,
                          KNOWN (IRI (expand (store, name))),
                          WILDCARD))
        end
                   
    fun text_list args =
        List.map (fn (_, _, LITERAL { value, ... }) => value)
                 (List.filter (fn (_, _, LITERAL _) => true
                              | (_, _, _) => false)
                              (match args))

    fun text args =
        case text_list args of
            result::_ => result
          | [] => ""

    fun iri_list args = 
        List.map (fn (_, _, IRI iri) => iri)
                 (List.filter (fn (_, _, IRI _) => true
                              | (_, _, _) => false)
                              (match args))

    fun iri args =
        case iri_list args of
            result::_ => SOME result
          | [] => NONE

    fun node_list args =
        List.map (fn (_, _, node) => node)
                 (List.filter (fn (_, _, IRI _) => true
                              | (_, _, BLANK _) => true
                              | (_, _, _) => false)
                              (match args))

    fun node args =
        case node_list args of
            result::_ => SOME result
          | [] => NONE
                     
end


