
structure TurtleCodepoints = struct

    open CodepointSet

    val alphaLower =
        withName "lower-case alphanumeric"
                  (fromAsciiRange #"a" #"z")

    val alpha =
	withName "alphanumeric"
                  (union [
	                fromAsciiRange #"A" #"Z",
	                alphaLower
	          ])

    val digit =
        withName "digit"
                  (fromAsciiRange #"0" #"9")
                  
    val exponent =
        withName "exponent"
                  (fromString "eE")

    val unicodeU =
        withName "character u"
                  (fromString "uU")
                  
    val numberAfterPoint =
        withName "numeric character"
	          (union [
	                digit,
                        exponent
                  ])

    val number =
        withName "numeric character or sign"
	          (union [
                        numberAfterPoint,
                        fromString "+-"
                  ])
                  
    val hex =
        withName "hex digit"
	          (union [
                        digit,
	                fromAsciiRange #"A" #"F",
	                fromAsciiRange #"a" #"f" 
	            ])

    val whitespace =
        withName "whitespace"
                  (fromString "\t ")

    val eol =
        withName "end of line"
                  (fromString "\n\r")

    val whitespaceEol =
        withName "whitespace or end of line"
                  (union [
                        whitespace,
                        eol
                  ])

    val comment =
        withName "comment character"
                  (fromString "#")
                  
    val basePnameChar =
        withName "initial prefixed-name character"
                  (union [
	                alpha,
	                fromRange 0wx00C0 0wx00D6,
	                fromRange 0wx00D8 0wx00F6,
	                fromRange 0wx00F8 0wx02FF,
	                fromRange 0wx0370 0wx037D,
	                fromRange 0wx037F 0wx1FFF,
	                fromRange 0wx200C 0wx200D,
	                fromRange 0wx2070 0wx218F,
	                fromRange 0wx2C00 0wx2FEF,
	                fromRange 0wx3001 0wxD7FF,
	                fromRange 0wxF900 0wxFDCF,
	                fromRange 0wxFDF0 0wxFFFD
	            ])

    val basePnameCharUscore =
        withName "initial local part character"
	          (union [
	                basePnameChar,
	                fromString "_"
	          ])

    val pnameChar =
        withName "prefixed-name character"
	          (union [
	                basePnameCharUscore,
                        digit,
	                fromWord 0wx00B7,
	                fromRange 0wx0300 0wx036F,
	                fromRange 0wx203F 0wx2040,
	                fromString "-"
	            ])

    val pnameCharInitialLocal =
        withName "initial char in prefixed-name local part"
                  (union [
                        basePnameCharUscore,
                        digit,
                        fromString ":"
                  ])
                  
    val pnameCharOrDot =
        withName "prefixed-name character or dot"
		  (union [
			pnameChar,
			fromString "."
		    ])

    val pnameCharOrColon =
        withName "prefixed-name character or colon"
		  (union [
			pnameChar,
			fromString ":"
		    ])

    val pnameCharColonOrDot =
        withName "prefixed-name character, colon, or dot"
		  (union [
			pnameChar,
			fromString ".:"
		    ])

    val pnameCharBackslash =
        withName "prefixed-name backslash escape"
		  (fromString "\\")

    val pnameCharPercent =
        withName "prefixed-name percent escape"
		  (fromString "%")

    val initialBnodeChar =
        withName "initial unescaped blank node character"
	          (union [
	                basePnameCharUscore,
                        digit
	          ])

    val iriExcluded =
        withName "character not permitted in iri ref"
                  (union [
	                fromRange 0wx0000 0wx0020,
	                fromString "<>\"{}|^`\\"
	            ])
	
    val pnameLocalToEscape =
        withName "character that must be escaped in local part"
                  (* not sure about this one? we definitely don't have
                     to escape e.g. underscore, even though we can *)
                  (fromString "~!$&'()*+,;=/?#@-")
	
    val pnameLocalEscapable =
        withName "character that can be escaped in local part"
                  (union [
                        pnameLocalToEscape,
                        fromString "_%."
                  ])

    val pnameExcluded =
	withName "prefixed-name excluded character"
		  (fromString "#;,)].\\\n\t\r ")

    val pnameAfterDot =
        withName "prefixed-name candidate character following dot"
                  (union [
                        pnameChar,
                        fromString ":%.\\"
                  ])

    val notALiteral =
        withName "initial character from non-literal value"
                  (union [
                        basePnameChar,
                        fromString "_([<:"
                  ])

    val shortStringSingleExcluded =
	withName "syntactic character in single-quoted string"
		  (fromString "\n\r'\\")

    val shortStringDoubleExcluded =
	withName "syntactic character in double-quoted string"
		  (fromString "\n\r\"\\")

    val longStringSingleExcluded =
	withName "syntactic character in long single-quoted string"
		  (fromString "'\\")

    val longStringDoubleExcluded =
	withName "syntactic character in long double-quoted string"
		  (fromString "\"\\")

    val stringEscape =
	withName "character that can be escaped in string"
		  (fromString "tbnrf\\\"'")

    structure CharMap = RedBlackMapFn (struct
                                        type ord_key = Word.word
                                        val compare = Word.compare
                                        end)

    val stringEscapeMap =
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

    datatype turtleSignificantChar =
             C_UNDERSCORE |
             C_OPEN_PAREN |
             C_OPEN_SQUARE |
             C_OPEN_ANGLE |
             C_CLOSE_PAREN |
             C_CLOSE_SQUARE |
             C_CLOSE_ANGLE |
             C_PERCENT |
             C_BACKSLASH |
             C_QUOTE_DOUBLE |
             C_QUOTE_SINGLE |
             C_DASH |
             C_AT |
             C_CARET |
             C_DOT |
             C_COMMA |
             C_COLON |
             C_SEMICOLON |
             C_LC_U |      (* for Unicode escapes *)
             C_UC_U |      (* for Unicode escapes *)
             C_LETTER_B |  (* for SPARQL-style base *)
             C_LETTER_P |  (* for SPARQL-style prefix *)
             C_LETTER_T |  (* for true *)
             C_LETTER_F |  (* for false *)
             C_NOTHING_INTERESTING

    val significantCharMap =
        let val pairings = [
             ( [ #"_"  ], C_UNDERSCORE ),
             ( [ #"("  ], C_OPEN_PAREN ),
             ( [ #"["  ], C_OPEN_SQUARE ),
             ( [ #"<"  ], C_OPEN_ANGLE ),
             ( [ #")"  ], C_CLOSE_PAREN ),
             ( [ #"]"  ], C_CLOSE_SQUARE ),
             ( [ #">"  ], C_CLOSE_ANGLE ),
             ( [ #"%"  ], C_PERCENT ),
             ( [ #"\\" ], C_BACKSLASH ),
             ( [ #"\"" ], C_QUOTE_DOUBLE ),
             ( [ #"'"  ], C_QUOTE_SINGLE ),
             ( [ #"-"  ], C_DASH ),
             ( [ #"@"  ], C_AT ),
             ( [ #"^"  ], C_CARET ),
             ( [ #"."  ], C_DOT ),
             ( [ #","  ], C_COMMA ),
             ( [ #":"  ], C_COLON ),
             ( [ #";"  ], C_SEMICOLON ),
             ( [ #"u"  ], C_LC_U ),
             ( [ #"U"  ], C_UC_U ),
             ( [ #"B", #"b" ], C_LETTER_B ),
             ( [ #"P", #"p" ], C_LETTER_P ),
             ( [ #"t"  ], C_LETTER_T ),  (* true and false are lower-case only *)
             ( [ #"f"  ], C_LETTER_F )
            ]
            fun ascii f = Word.fromInt (Char.ord f)
        in
            List.foldl
                (fn ((fromList, to), map) =>
                    List.foldl (fn (from, map) => 
                                   CharMap.insert (map, ascii from, to))
                               map fromList)
                CharMap.empty pairings
        end
            
    fun significantCharName c =
        (* Slow, but that doesn't matter as long as this is only used once 
           for constructing an error message *)
        case List.filter (fn (k, v) => v = c)
                         (CharMap.listItemsi significantCharMap) of
            [] => raise Fail "internal error: unknown character"
          | pairs =>
            String.concatWith " or "
                              (List.map
                                   (fn (w, _) =>
                                       "\"" ^
                                       (WdString.implodeToUtf8 [w]) ^
                                       "\"")
                                   pairs)
        
end
                           
