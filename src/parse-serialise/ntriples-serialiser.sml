
structure NTriplesSerialiser :> RDF_INCREMENTAL_SERIALISER = struct

    open RdfTriple

    type t = CodepointIO.outstream

    fun new t = t

    fun encodeIri iri =
        Encode.encodeWdstring (NTriplesCodepoints.okInIris,
                                Encode.percentOrUEncode)
                               (Iri.toWideString iri)
            
    fun encodeLiteralValue value =
        Encode.encodeString (NTriplesCodepoints.okInStrings,
                              Encode.asciiEncode)
                             value
				  
    fun stringOfNode (RdfNode.IRI iri) = "<" ^ (encodeIri iri) ^ ">"
      | stringOfNode (RdfNode.BLANK n) = "_:blank" ^ (Int.toString n)
      | stringOfNode (RdfNode.LITERAL lit) =
	"\"" ^ (encodeLiteralValue (#value lit)) ^ "\"" ^
        (if #lang lit = ""
         then ""
         else "@" ^ (#lang lit)) ^
        (if Iri.isEmpty (#dtype lit)
            orelse (#dtype lit) = RdfStandardIRIs.iriTypeString
         then ""
	 else "^^" ^ (stringOfNode (RdfNode.IRI (#dtype lit))))
	    
    (** Add a series of triples to the output. *)
    fun serialise (t, []) = t
      | serialise (t, (a, b, c)::rest) =
	let val str = stringOfNode a ^ " " ^
		      stringOfNode b ^ " " ^
		      stringOfNode c
	in
	    CodepointIO.outputUtf8 (t, str ^ " .\n");
	    serialise (t, rest)
	end

    (** Write any pending triples and clean up. 
        Does not close the underlying text stream. *)
    fun finish t = ()
            
end
							    
