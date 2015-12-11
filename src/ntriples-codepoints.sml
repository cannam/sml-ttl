
structure NTriplesCodepoints = struct

    open CodepointSet

    val ok_in_strings =
        with_name "ascii characters allowed unescaped in a string literal"
                  (union [
	                from_range 0wx0020 0wx0021,
	                from_range 0wx0023 0wx005b,
	                from_range 0wx005d 0wx007e
		    ])

    val ok_in_iris =
	with_name "ascii characters allowed unescaped in an iri"
		  (union [
			from_ascii_range #"A" #"Z",
			from_ascii_range #"a" #"z",
			from_ascii_range #"0" #"9",
			from_string "!#$&'()*+,./:;=?@[]_~-"
		    ])

end
				   
