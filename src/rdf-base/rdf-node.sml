			 
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

    val bnodeCounter = ref 0
		
    fun newBlankNode () =
	let val id = !bnodeCounter in
	    bnodeCounter := id + 1;
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
                               
    fun stringOfNode (IRI iri) = "<" ^ (Iri.toString iri) ^ ">"
      | stringOfNode (BLANK n) = "_:blank" ^ (Int.toString n)
      | stringOfNode (LITERAL lit) =
        "\"" ^ (#value lit) ^ "\"" ^
        (if #dtype lit = Iri.empty then ""
	 else "^^" ^ (Iri.toString (#dtype lit))) ^
        (if #lang lit = "" then ""
	 else "@" ^ (#lang lit))
    
end
