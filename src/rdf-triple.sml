
signature RDF_NODE = sig

    datatype node =
             IRI of string |
             BLANK of int |
             LITERAL of {
		 value : string,
		 dtype : string,
		 lang  : string
             }

    val new_blank_node : unit -> node

end
			 
structure RdfNode :> RDF_NODE = struct

    datatype node =
             IRI of string |
             BLANK of int |
             LITERAL of {
		 value : string,
		 dtype : string,
		 lang  : string
             }

    val bnode_counter = ref 0
		
    fun new_blank_node () =
	let val id = !bnode_counter in
	    bnode_counter := id + 1;
	    BLANK id
	end
    
end
		    
structure RdfTriple = struct

    open RdfNode
			    
    type triple = node * node * node

    type prefix = string * string

    fun string_of_node (IRI iri) = "<" ^ iri ^ ">"
      | string_of_node (BLANK n) = "_" ^ (Int.toString n)
      | string_of_node (LITERAL lit) = "\"" ^ (#value lit) ^ "\"" ^
                                       (if #dtype lit = "" then ""
					else "^^" ^ (#dtype lit)) ^
                                       (if #lang lit = "" then ""
					else "@" ^ (#lang lit))

    fun string_of_triple (a,b,c) =
	"(" ^ (string_of_node a) ^
	"," ^ (string_of_node b) ^
	"," ^ (string_of_node c) ^
	")"

end

