
structure Rdf = struct

    type iri = Iri.t

    datatype node =
             IRI of iri |
             BLANK of int |
             LITERAL of {
		 value : string,
		 dtype : iri,
		 lang  : string
             }

    type triple = node * node * node

    type prefix = string * string

    fun string_of_node (IRI iri) = "<" ^ (Iri.toString iri) ^ ">"
      | string_of_node (BLANK n) = "_" ^ (Int.toString n)
      | string_of_node (LITERAL lit) =
        "\"" ^ (#value lit) ^ "\"" ^
        (if Iri.equals (#dtype lit, Iri.empty_iri) then ""
	 else "^^" ^ (Iri.toString (#dtype lit))) ^
        (if #lang lit = "" then ""
	 else "@" ^ (#lang lit))

    fun string_of_triple (a,b,c) =
	"(" ^ (string_of_node a) ^
	"," ^ (string_of_node b) ^
	"," ^ (string_of_node c) ^
	")"

end

