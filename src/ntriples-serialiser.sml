
structure NTriplesSerialiser :> RDF_STREAM_SERIALISER = struct

    open RdfTriple
    open NTriplesEncoders

    datatype entry =
	     PREFIX of prefix |
	     TRIPLE of triple

    type t = TextIO.outstream

    fun new t = t
	    
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
							    
