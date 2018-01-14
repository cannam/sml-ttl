
functor TurtleSerialiserFn (ARG : sig
                                structure P : PREFIX_TABLE
                                structure M : MATCHER
                            end)
        :> RDF_ABBREVIATING_SERIALISER
               where type prefix_table = ARG.P.t
               where type matcher = ARG.M.t
= struct

    open RdfNode

    type base_iri = BaseIri.t
             
    structure PrefixTable = ARG.P
    structure Matcher = ARG.M
    structure CollectionGatherer = CollectionGathererFn(Matcher)

    structure Triples = RedBlackSetFn (struct
			                type ord_key = RdfTriple.triple
			                val compare = RdfTriple.compare
			                end)

    type ser_data = {
        stream    : TextIO.outstream,
        base      : string option,
        subject   : RdfNode.node option,
        predicate : RdfNode.node option,
        indent    : int,
        written   : Triples.set,
        prefixes  : PrefixTable.t,
        matcher   : Matcher.t
    }

    type ser_props = {
        hasAnonymousSubject : bool,
        hasAnonymousObject : bool,
        hasCollectionObject : bool
    }        

    fun wasWritten (triple, d : ser_data) = Triples.member (#written d, triple)

    fun encodeLocal str =
        Encode.encodeStringExcept (TurtleCodepoints.pnameLocalToEscape,
                                     Encode.backslashEncode)
                                    str

    fun encodeIri base_iri iri =
        let val istr = Iri.toString iri
        in
            case base_iri of
                NONE => istr
              | SOME bstr => if String.isPrefix bstr istr
                             then implode (List.drop (explode istr,
                                                      String.size bstr))
                             else istr
        end
            
    fun stringOfAbbrIri (iri, d : ser_data) =
	if iri = RdfStandardIRIs.iriRdfType then "a"
        else if iri = RdfStandardIRIs.iriRdfNil then "()"
	else case PrefixTable.abbreviate (#prefixes d, iri) of
                 SOME (ns, rest) => ns ^ ":" ^ encodeLocal rest
               | NONE => "<" ^ (encodeIri (#base d) iri) ^ ">"

    fun stringForIndent indent =
        if indent < 0 then ""
        else String.concatWith "" (List.tabulate (indent * 4, fn _ => " "))

    fun write (d : ser_data, str) =
        (TextIO.output (#stream d, str); d)
                               
    fun writeIndent (d : ser_data) =
        write (d, stringForIndent (#indent d))
              
    fun shouldUseLongString (lit : RdfNode.literal) =
        String.size (#value lit) > 80 (* arbitrary, avoid scanning v long strings *)
        orelse
        List.exists
            (fn w => CodepointSet.contains
                         TurtleCodepoints.shortStringDoubleExcluded
                         w)
            (WdString.explodeUtf8 (#value lit))
                          
    fun serialiseNodes (d : ser_data, p : ser_props) nodes =
        case foldl (fn (n, (d, sep)) =>
                       (serialiseAbbreviated (n, write (d, sep)), " "))
                   (writeIndent d, "")
                   nodes
         of (d, _) => d

    and serialiseAnonymousObject (obj, d) =
        let val triples = Matcher.match (#matcher d, (SOME obj, NONE, NONE))
            val d = write (d, "[\n")
            val d =
                indented (~1)
                         (serialiseTriples {
                               stream = #stream d,
                               base = #base d,
                               subject = SOME obj,
                               predicate = NONE,
                               indent = 1 + #indent d,
                               written = #written d,
                               prefixes = #prefixes d,
                               matcher = #matcher d
                           } triples)
        in
            write (writeIndent (write (d, "\n")), "]")
        end

    and serialiseStringLiteral (lit, d) =
        let val long = shouldUseLongString lit
            val quote = if long then "\"\"\"" else "\""
            val d = write (write (d, quote),
                           Encode.encodeStringExcept
                               (if shouldUseLongString lit
                                then (TurtleCodepoints.longStringDoubleEscaped,
                                      Encode.backslashEncode)
                                else (TurtleCodepoints.shortStringDoubleExcluded,
                                      Encode.asciiEncode))
                               (#value lit))
            val d = write (d, quote)
            val d = if #lang lit = "" then d
                    else write (d, "@" ^ (#lang lit))
            val d = if Iri.isEmpty (#dtype lit)
                       orelse (#dtype lit) = RdfStandardIRIs.iriTypeString 
                    then d
                    else write (d, "^^" ^ stringOfAbbrIri (#dtype lit, d))
        in
            d
        end

    and serialiseBooleanLiteral (lit, d) =
        case #value lit of
            "true" => write (d, "true")
          | "false" => write (d, "false")
          | _ => serialiseStringLiteral (lit, d)

    and serialiseIntegerLiteral (lit, d) =
        if List.find (fn c => not (Char.isDigit c))
                     (String.explode (#value lit)) = NONE
        then write (d, #value lit)
        else serialiseStringLiteral (lit, d)
                                          
    and serialiseAbbreviated (node, d : ser_data) : ser_data =
        case node of
            IRI iri => write (d, stringOfAbbrIri (iri, d))
          | BLANK n => write (d, "_:blank" ^ (Int.toString n))
          | LITERAL (lit as { dtype, ... }) =>
            if dtype = RdfStandardIRIs.iriTypeBoolean
            then serialiseBooleanLiteral (lit, d)
            else if dtype = RdfStandardIRIs.iriTypeInteger
            then serialiseIntegerLiteral (lit, d)
            (*!!! + double, decimal *)
            else serialiseStringLiteral (lit, d)

    and serialiseObject (obj, d, p : ser_props) : ser_data =
        if (#hasAnonymousObject p)
        then serialiseAnonymousObject (obj, write (d, " "))
        else serialiseAbbreviated (obj, write (d, " "))

    and serialiseCollection (obj, d, p) =
        let val triples = CollectionGatherer.triplesOfCollection
                              (#matcher d, obj)
            val anyWritten = List.exists (fn t => wasWritten (t, d)) triples
        in
            if anyWritten
            then
                (* Can't write as a collection, because one or more of
                   its metanodes have already been written. This could be
                   because the head of the collection is a subject node
                   rather than an object. *)
                serialiseObject (obj, d, p)
            else
                let val d = write (d, " (")
                    val d = 
                        foldl (fn (t as (_, pred, obj) : RdfTriple.triple, d) =>
                                  let val d = {
                                          stream = #stream d,
                                          base = #base d,
                                          subject = #subject d,
                                          predicate = #predicate d,
                                          indent = #indent d,
                                          written = Triples.add (#written d, t),
                                          prefixes = #prefixes d,
                                          matcher = #matcher d
                                      }
                                  in
                                      if pred = IRI RdfStandardIRIs.iriRdfFirst
                                      then
                                          (TextIO.output (#stream d, " ");
                                           serialiseAbbreviated (obj, d))
                                      else d
                                  end)
                              d triples
                in
                    write (d, " )")
                end
        end
                 
    and serialiseObjectOrCollection (obj, d, p : ser_props) =
        if (#hasCollectionObject p)
        then serialiseCollection (obj, d, p)
        else serialiseObject (obj, d, p)

    and indented n (d : ser_data) =
        { stream = #stream d,
          base = #base d,
          subject = #subject d,
          predicate = #predicate d,
          indent = n + #indent d,
          written = #written d,
          prefixes = #prefixes d,
          matcher = #matcher d }
                 
    and serialiseSubject (subj, d : ser_data, p : ser_props) =
        if #hasAnonymousSubject p
        then write (d, "[]")
        else serialiseNodes (d, p) [subj]
                 
    and serialiseSubjectPredicate (subj, pred, d : ser_data, p : ser_props) =
        case #subject d of
            NONE =>
            (* first triple in graph *)
            indented 1 (serialiseNodes
                            (write (serialiseSubject (subj, d, p), " "), p)
                            [pred])
          | SOME currentSubj =>
            if currentSubj = subj then
                case #predicate d of
                    NONE =>
                    (* first triple in bnode [...] syntax *)
                    serialiseNodes (d, p) [pred]
                  | SOME currentPred =>
                    if currentPred = pred then write (d, ",")
                    else serialiseNodes (write (d, " ;\n"), p) [pred]
            else
                let val d = indented (~1) (write (d, " .\n\n"))
                in
                    indented
                        1 (serialiseNodes
                               (write (serialiseSubject (subj, d, p), " "), p)
                               [pred])
                end

    and serialiseTripleParts ((subj, pred, obj), d : ser_data, p : ser_props) =
        let val d = serialiseSubjectPredicate (subj, pred, d, p)
            val d = serialiseObjectOrCollection (obj, d, p)
        in
            { stream = #stream d,
              base = #base d,
              subject = SOME subj,
              predicate = SOME pred,
              indent = #indent d,
              written = #written d,
              prefixes = #prefixes d,
              matcher = #matcher d }
        end

    and serialiseTripleMaybe (triple, d) =
        let
            fun hasBlankSubject (BLANK _, _, _) = true
              | hasBlankSubject _ = false

            fun hasBlankObject (_, _, BLANK _) = true
              | hasBlankObject _ = false
       
            fun isBlankOnlyAsSubject (d : ser_data, t : RdfTriple.triple) =
                Matcher.match (#matcher d, (NONE, NONE, SOME (#1 t))) = []
                andalso
                Matcher.match (#matcher d, (NONE, SOME (#1 t), NONE)) = []

            fun isBlankObjectUnique (d : ser_data, t : RdfTriple.triple) =
                Matcher.match (#matcher d, (NONE, NONE, SOME (#3 t))) = [t]

            fun wasBlankNodeWritten (d : ser_data, node) =
                List.exists
                    (fn x => wasWritten (x, d))
                    (Matcher.match (#matcher d, (SOME node, NONE, NONE)))

            fun isBlankNodeUnwritten args = not (wasBlankNodeWritten args)

            val hasAnonymousSubject = hasBlankSubject triple andalso
                                      isBlankOnlyAsSubject (d, triple)
                                                            
            val hasAnonymousObject = hasBlankObject triple andalso
                                     isBlankObjectUnique (d, triple) andalso
                                     isBlankNodeUnwritten (d, #3 triple)
                                                          
            val hasCollectionObject = hasAnonymousObject andalso
                                      CollectionGatherer.isCollectionNode
                                          (#matcher d, #3 triple)
        in
            if wasWritten (triple, d) then d
            else
                serialiseTripleParts
                    (triple,
                     { stream = #stream d,
                       base = #base d,
                       subject = #subject d,
                       predicate = #predicate d,
                       indent = #indent d,
                       written = Triples.add (#written d, triple),
                       prefixes = #prefixes d,
                       matcher = #matcher d },
                     { hasAnonymousSubject = hasAnonymousSubject,
                       hasAnonymousObject = hasAnonymousObject,
                       hasCollectionObject = hasCollectionObject })
        end
                        
    and serialiseTriples data triples =
        foldl (fn (t, d) => serialiseTripleMaybe (t, d))
              data
              (* This sorts by internal key, which has the nice
                 property that if we read in a series of triples and
                 then write them out again we will usually get the
                 same ordering in the output as the input. Iri-subject
                 triples get sorted before blanks. *)
              (ListMergeSort.sort
                   (fn args => RdfTriple.compare args = GREATER)
                   triples)
        
    and serialisePrefixes stream prefixes =
        foldl (fn ((pfx, iri), t) =>
                  (TextIO.output (t, "@prefix " ^ pfx ^ ": <" ^
                                     (Iri.toString iri) ^ "> .\n");
                   t))
              stream prefixes

    and serialiseBase stream NONE = stream
      | serialiseBase stream (SOME iri) = 
        (TextIO.output (stream, "@base <" ^ (Iri.toString iri) ^ "> .\n");
         stream)

    fun new (base_iri, prefixes, matcher) stream =
        let val stream = serialisePrefixes
                             (serialiseBase stream base_iri)
                             (PrefixTable.enumerate prefixes)
            val _ = TextIO.output (stream, "\n")
        in
            { stream = stream,
              base = case base_iri of
                         NONE => NONE
                       | SOME iri => if Iri.isEmpty iri
                                     then NONE
                                     else SOME (Iri.toString iri),
              subject = NONE,
              predicate = NONE,
              indent = 0,
              written = Triples.empty,
              prefixes = prefixes,
              matcher = matcher }
        end

    fun serialise (d, triples) =
        serialiseTriples d triples

    fun finish (d : ser_data) =
        if not (Triples.isEmpty (#written d))
        then TextIO.output (#stream d, " .\n")
        else ()

    type t = ser_data
    type triple = RdfTriple.triple
    type prefix_table = ARG.P.t
    type matcher = ARG.M.t
end

                                                   
