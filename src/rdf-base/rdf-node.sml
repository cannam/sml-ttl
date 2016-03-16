			 
structure RdfNode :> RDF_NODE where type iri = Iri.t = struct

    type iri = Iri.t

    type literal = {
	value : string,
	dtype : iri,
	lang  : string
    }
		   
    datatype node =
             IRI of iri |
             BLANK of int |
             LITERAL of literal

    val bnode_counter = ref 0
		
    fun new_blank_node () =
	let val id = !bnode_counter in
	    bnode_counter := id + 1;
	    BLANK id
	end

    fun compare (IRI i1, IRI i2) = Iri.compare (i1, i2) 
      | compare (BLANK b1, BLANK b2) = Int.compare (b1, b2)
      | compare (LITERAL l1, LITERAL l2) =
        (case String.compare (#value l1, #value l2) of
             GREATER => GREATER
           | LESS => LESS
           | EQUAL =>
             case Iri.compare (#dtype l1, #dtype l2) of
                 GREATER => GREATER
               | LESS => LESS
               | EQUAL => String.compare (#lang l1, #lang l2))
      | compare (IRI _, _) = LESS
      | compare (BLANK _, IRI _) = GREATER
      | compare (BLANK _, _) = LESS
      | compare (LITERAL _, _) = GREATER
                               
    fun string_of_node (IRI iri) = "<" ^ (Iri.toString iri) ^ ">"
      | string_of_node (BLANK n) = "_:blank" ^ (Int.toString n)
      | string_of_node (LITERAL lit) =
        "\"" ^ (#value lit) ^ "\"" ^
        (if #dtype lit = Iri.empty then ""
	 else "^^" ^ (Iri.toString (#dtype lit))) ^
        (if #lang lit = "" then ""
	 else "@" ^ (#lang lit))
    
end
