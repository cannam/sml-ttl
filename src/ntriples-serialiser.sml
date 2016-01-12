
structure NTriplesSerialiser :> RDF_STREAM_SERIALISER = struct

    open RdfTriple

    datatype entry =
	     PREFIX of prefix |
	     TRIPLE of triple

    type t = TextIO.outstream

    fun new t = t

    fun encode_iri iri =
        Encode.encode_wdstring (NTriplesCodepoints.ok_in_iris,
                                Encode.percent_or_u_encode)
                               (Iri.toWideString iri)
            
    fun encode_literal_value value =
        Encode.encode_string (NTriplesCodepoints.ok_in_strings,
                              Encode.ascii_encode)
                             value
				  
    fun string_of_node (RdfNode.IRI iri) = "<" ^ (encode_iri iri) ^ ">"
      | string_of_node (RdfNode.BLANK n) = "_:blank" ^ (Int.toString n)
      | string_of_node (RdfNode.LITERAL lit) =
	"\"" ^ (encode_literal_value (#value lit)) ^ "\"" ^
        (if #lang lit = "" then "" else "@" ^ (#lang lit)) ^
        (if Iri.is_empty (#dtype lit) then ""
	 else "^^" ^ (string_of_node (RdfNode.IRI (#dtype lit))))
	    
    fun serialise (t, PREFIX prefix) = t  (* ntriples doesn't include prefixes *)
      | serialise (t, TRIPLE (a, b, c)) =
	let val str = string_of_node a ^ " " ^
		      string_of_node b ^ " " ^
		      string_of_node c
	in
	    TextIO.output (t, str ^ " .\n");
	    t
	end
		 
end
							    
