
structure NTriplesSerialiser :> RDF_STREAM_SERIALISER = struct

    open RdfTriple

    datatype entry =
	     PREFIX of prefix |
	     TRIPLE of triple

    type t = TextIO.outstream

    fun new t = t

    fun hex_string_of w min =
        let fun padded h =
                let val len = String.size h in
                    if len < min then padded ("0" ^ h)
                    else if Int.mod (String.size h, 2) = 1 then "0" ^ h
                    else h
                end
        in
            padded (Word.toString w)
        end
                                   
    fun u_encode w =
        SimpleWideString.explodeUtf8
            (if w > 0wxffff
             then "\\U" ^ (hex_string_of w 8)
             else "\\u" ^ (hex_string_of w 4))
                                        
    fun percent_encode w = SimpleWideString.explodeUtf8 ("%" ^ (hex_string_of w 0))

    fun percent_or_u_encode w =
        if w > 0wx00ff then u_encode w
        else percent_encode w
                                              
    fun ascii_encode 0wx09 = SimpleWideString.explodeUtf8 "\\t"
      | ascii_encode 0wx0A = SimpleWideString.explodeUtf8 "\\n"
      | ascii_encode 0wx0D = SimpleWideString.explodeUtf8 "\\r"
      | ascii_encode 0wx22 = SimpleWideString.explodeUtf8 "\\\""
      | ascii_encode 0wx5C = SimpleWideString.explodeUtf8 "\\\\"
      | ascii_encode w = u_encode w

    fun encode_as_token acceptable encoder token =
        let fun encode' [] = []
              | encode' (w::ws) = 
                if CodepointSet.contains acceptable w
                then w :: encode' ws
                else (encoder w) @ encode' ws
        in
            SimpleWideString.implodeToUtf8 (encode' (SimpleWideString.explode token))
        end
                                  
    fun encode_iri iri =
        encode_as_token NTriplesCodepoints.ok_in_iris
                        percent_or_u_encode
                        (Iri.toWideString iri)
            
    fun encode_literal_value value =
        encode_as_token NTriplesCodepoints.ok_in_strings
                        ascii_encode
                        (SimpleWideString.fromUtf8 value)
				  
    fun string_of_node (IRI iri) = "<" ^ (encode_iri iri) ^ ">"
      | string_of_node (BLANK n) = "_:blank" ^ (Int.toString n)
      | string_of_node (LITERAL lit) =
	"\"" ^ (encode_literal_value (#value lit)) ^ "\"" ^
        (if #lang lit = "" then "" else "@" ^ (#lang lit)) ^
        (if Iri.is_empty (#dtype lit) then ""
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
							    
