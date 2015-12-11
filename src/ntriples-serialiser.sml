
structure NTriplesSerialiser :> RDF_STREAM_SERIALISER = struct

    open RdfTriple

    datatype entry =
	     PREFIX of prefix |
	     TRIPLE of triple

    type t = TextIO.outstream

    fun create_for_stream t = t

    fun encode_iri iri = iri (* !!! use NTriplesCodepoints.ok_in_iris *)
    fun encode_literal_value blah = blah (* !!! use NTriplesCodepoints.ok_in_strings *)
				  
    fun string_of_node (IRI iri) = "<" ^ (encode_iri iri) ^ ">"
      | string_of_node (BLANK n) = "_:blank" ^ (Int.toString n)
      | string_of_node (LITERAL lit) =
	"\"" ^ (encode_literal_value (#value lit)) ^ "\"" ^
        (if #lang lit = "" then "" else "@" ^ (#lang lit)) ^
        (if #dtype lit = "" then ""
	 else "^^" ^ (string_of_node (IRI (#dtype lit))))
	    
    fun serialise (t, PREFIX prefix) = t  (* ntriples doesn't include prefixes *)
      | serialise (t, TRIPLE (a, b, c)) =
	let val str = string_of_node a ^ " " ^
		      string_of_node b ^ " " ^
		      string_of_node c
	in
	    TextIO.output (t, str ^ ".\n");
	    t
	end
		 
end
							    
