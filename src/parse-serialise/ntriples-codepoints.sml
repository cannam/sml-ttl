
structure NTriplesCodepoints = struct

    open CodepointSet

    val okInStrings =
        withName "ascii characters allowed unescaped in a string literal"
                  (union [
	                fromRange 0wx0020 0wx0021,
	                fromRange 0wx0023 0wx005b,
	                fromRange 0wx005d 0wx007e
		    ])

    val okInIris =
	withName "ascii characters allowed unescaped in an iri"
		  (union [
			fromAsciiRange #"A" #"Z",
			fromAsciiRange #"a" #"z",
			fromAsciiRange #"0" #"9",
			fromString "!#$&'()*+,./:;=?@[]_~-"
		    ])

end
				   
