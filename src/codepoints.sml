
structure Codepoints = struct

    open CodepointSet

    val alpha_lower =
        with_name "lower-case alphanumeric"
                  (from_ascii_range #"a" #"z")

    val alpha =
	with_name "alphanumeric"
                  (union [
	                from_ascii_range #"A" #"Z",
	                alpha_lower
	          ])

    val exponent =
        with_name "exponent"
                  (from_string "eE")

    val unicode_u =
        with_name "character u"
                  (from_string "uU")
                  
    val number_after_point =
        with_name "numeric character"
	          (union [
	                from_ascii_range #"0" #"9",
                        exponent
                  ])

    val number =
        with_name "numeric character or sign"
	          (union [
                        number_after_point,
                        from_string "+-"
                  ])
                  
    val hex =
        with_name "hex digit"
	          (union [
	                from_ascii_range #"0" #"9",
	                from_ascii_range #"A" #"F",
	                from_ascii_range #"a" #"f" 
	            ])

    val whitespace =
        with_name "whitespace"
                  (from_string "\t ")

    val eol =
        with_name "end of line"
                  (from_string "\n\r")

    val whitespace_eol =
        with_name "whitespace or end of line"
                  (union [
                        whitespace,
                        eol
                  ])

    val comment =
        with_name "comment character"
                  (from_string "#")
                  
    val base_pname_char =
        with_name "initial prefixed-name character"
                  (union [
	                alpha,
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

    val base_pname_char_uscore =
        with_name "initial local part character"
	          (union [
	                base_pname_char,
	                from_string "_"
	          ])

    val pname_char =
        with_name "prefixed-name character"
	          (union [
	                base_pname_char_uscore,
	                from_ascii_range #"0" #"9",
	                from_word 0wx00B7,
	                from_range 0wx0300 0wx036F,
	                from_range 0wx203F 0wx2040,
	                from_string "-"
	            ])

    val pname_char_or_dot =
        with_name "prefixed-name character or dot"
		  (union [
			pname_char,
			from_string "."
		    ])

    val initial_bnode_char =
        with_name "initial blank node character"
	          (union [
	                base_pname_char_uscore,
	                from_ascii_range #"0" #"9"
	          ])

    val iri_escaped =
        with_name "iri escaped character"
                  (union [
	                from_range 0wx0000 0wx0020,
	                from_string "<>\"{}|^`\\%"
	            ])
	
    val pname_local_escapable =
        with_name "local part escaped character"
                  (from_string "_~.!$&'()*+,;=/?#@%-")

    val pname_definitely_excluded =
	with_name "prefixed-name excluded character"
		  (from_string "#;,)].\n\t\r ")

    val pname_after_dot =
        with_name "prefixed-name candidate character following dot"
                  (union [
                        pname_char,
                        from_string ":%.\\"
                  ])

    val not_a_literal =
        with_name "initial character from non-literal value"
                  (union [
                        base_pname_char,
                        from_string "_([<:"
                  ])

    val string_single_excluded =
	with_name "syntactic character in single-quoted string"
		  (from_string "\\\n\r'")

    val string_double_excluded =
	with_name "syntactic character in double-quoted string"
		  (from_string "\\\n\r\"")

    val string_escape =
	with_name "character that can be escaped in string"
		  (from_string "tbnrf\\\"'")

    structure CharMap = RedBlackMapFn (struct
                                        type ord_key = Word.word
                                        val compare = Word.compare
                                        end)

    val string_escape_map =
        let val pairings = [
                ( #"t", #"\t" ), ( #"b", #"\b" ), ( #"n", #"\n" ),
                ( #"r", #"\r" ), ( #"f", #"\f" ), ( #"\\", #"\\" ),
                ( #"\"", #"\"" ), ( #"'", #"'" ) ]
            fun ascii f = Word.fromInt (Char.ord f)
        in
            List.foldl
                (fn ((from, to), map) =>
                    CharMap.insert (map, ascii from, ascii to))
                CharMap.empty pairings
        end

    datatype turtle_significant_char =
             P_UNDERSCORE |
             P_OPEN_PAREN |
             P_OPEN_SQUARE |
             P_OPEN_ANGLE |
             P_PERCENT |
             P_BACKSLASH |
             P_QUOTE_DOUBLE |
             P_QUOTE_SINGLE |
             P_DASH |
             P_AT |
             P_CARET |
             P_DOT |
             P_LC_U |      (* for Unicode escapes *)
             P_UC_U |      (* for Unicode escapes *)
             P_LETTER_B |  (* for SPARQL-style base *)
             P_LETTER_P |  (* for SPARQL-style prefix *)
             P_LETTER_T |  (* for true *)
             P_LETTER_F |  (* for false *)
             P_NOTHING_INTERESTING

    val significant_char_map =
        let val pairings = [
             ( [ #"_"  ], P_UNDERSCORE ),
             ( [ #"("  ], P_OPEN_PAREN ),
             ( [ #"["  ], P_OPEN_SQUARE ),
             ( [ #"<"  ], P_OPEN_ANGLE ),
             ( [ #"%"  ], P_PERCENT ),
             ( [ #"\\" ], P_BACKSLASH ),
             ( [ #"\"" ], P_QUOTE_DOUBLE ),
             ( [ #"'"  ], P_QUOTE_SINGLE ),
             ( [ #"-"  ], P_DASH ),
             ( [ #"@"  ], P_AT ),
             ( [ #"^"  ], P_CARET ),
             ( [ #"."  ], P_DOT ),
             ( [ #"u"  ], P_LC_U ),
             ( [ #"U"  ], P_UC_U ),
             ( [ #"B", #"b" ], P_LETTER_B ),
             ( [ #"P", #"p" ], P_LETTER_P ),
             ( [ #"t"  ], P_LETTER_T ),  (* true and false are lower-case only *)
             ( [ #"f"  ], P_LETTER_F )
            ]
            fun ascii f = Word.fromInt (Char.ord f)
        in
            List.foldl
                (fn ((from_list, to), map) =>
                    List.foldl (fn (from, map) => 
                                   CharMap.insert (map, ascii from, to))
                               map from_list)
                CharMap.empty pairings
        end
            
            
end
                           
