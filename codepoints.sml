
structure Codepoints = struct

    open CodepointSet

    val alpha_lower_codepoints =
        with_name "lower-case alphanumeric"
                  (from_ascii_range #"a" #"z")

    val alpha_codepoints =
	with_name "alphanumeric"
                  (union [
	                from_ascii_range #"A" #"Z",
	                alpha_lower_codepoints
	          ])
                  
    val hex_codepoints =
        with_name "hex digit"
	          (union [
	                from_ascii_range #"0" #"9",
	                from_ascii_range #"A" #"F",
	                from_ascii_range #"a" #"f" 
	            ])

    val whitespace_codepoints =
        with_name "whitespace"
                  (from_string "\n\r\t ")

    val eol_codepoints =
        with_name "end of line"
                  (from_string "\n\r")
	
    val base_pname_char_codepoints =
        with_name "initial prefixed-name character"
                  (union [
	                alpha_codepoints,
	                from_range 0wx00C0 0wx00D6,
	                from_range 0wx00D8 0wx00F6,
	                from_range 0wx00F8 0wx02FF,
	                from_range 0wx0370 0wx037D,
	                from_range 0wx037F 0wx1FFF,
	                from_range 0wx200C 0wx200D,
	                from_range 0wx2070 0wx218F,
	                from_range 0wx2C00 0wx2FEF,
	                from_range 0wx3001 0wxD7FF,
	                from_range 0wxF900 0wxFDCF,
	                from_range 0wxFDF0 0wxFFFD
	            ])

    val base_pname_char_uscore_codepoints =
        with_name "initial local part character"
	          (union [
	                base_pname_char_codepoints,
	                from_string "_"
	          ])

    val pname_char_codepoints =
        with_name "prefixed-name character"
	          (union [
	                base_pname_char_uscore_codepoints,
	                from_ascii_range #"0" #"9",
	                from_word 0wx00B7,
	                from_range 0wx0300 0wx036F,
	                from_range 0wx203F 0wx2040,
	                from_string "-"
	            ])

    val iri_escaped_codepoints =
        with_name "iri escaped character"
                  (union [
	                from_range 0wx0000 0wx0020,
	                from_string "<>\"{}|^`\\%"
	            ])
	
    val pname_local_escapable_codepoints =
        with_name "local part escaped character"
                  (from_string "_~.!$&'()*+,;=/?#@%-")

end
                           
