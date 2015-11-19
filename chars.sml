
val base_pname_char_codepoints =
    let open Codepoints in
	union [
	    from_ascii_range #"A" #"Z",
	    from_ascii_range #"a" #"z",
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
	]
    end

val base_pname_char_u_codepoints =
    let open Codepoints in
	union [
	    base_pname_char_codepoints,
	    from_string "_"
	]
    end

val pname_char_codepoints =
    let open Codepoints in
	union [
	    base_pname_char_u_codepoints,
	    from_ascii_range #"0" #"9",
	    from_word 0wx00B7,
	    from_range 0wx0300 0wx036F,
	    from_range 0wx203F 0wx2040,
	    from_string "-"
	]
    end

val pname_local_escapable =
    Codepoints.from_string "_~.!$&'()*+,;=/?#@%-"
