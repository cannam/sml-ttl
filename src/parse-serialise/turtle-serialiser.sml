
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
             
    fun sortedBySubject triples = (* with iri-subject triples before blanks *)
        let fun greater (n1, n2) =
                RdfNode.compare (n1, n2) = GREATER
        in
            ListMergeSort.sort
                (fn ((s1, _, _), (s2, _, _)) => greater (s1, s2))
                triples
        end

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
        isAnon   : bool,
        isColl   : bool
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
	else case PrefixTable.abbreviate (#prefixes d, iri) of
                 SOME (ns, rest) => ns ^ ":" ^ encodeLocal rest
               | NONE => "<" ^ (encodeIri (#base d) iri) ^ ">"

    fun stringForIndent indent =
        if indent < 0 then ""
        else String.concatWith "" (List.tabulate (indent * 4, fn _ => " "))

    fun writeIndent (d : ser_data) =
        (TextIO.output (#stream d, stringForIndent (#indent d));
         d)
            
    fun shouldUseLongString (lit : RdfNode.literal) =
        String.size (#value lit) > 80 (* arbitrary, avoid scanning v long strings *)
        orelse
        List.exists
            (fn w => CodepointSet.contains
                         TurtleCodepoints.shortStringDoubleExcluded
                         w)
            (WdString.explodeUtf8 (#value lit))
                          
    fun serialiseNodes (d, pr) nodes =
        case foldl (fn (n, (d, sep)) =>
                       (TextIO.output (#stream d, sep);
                        serialiseAbbreviated (n, d);
                        (d, " ")))
                   ((writeIndent d), "")
                   nodes
         of (d, _) => d

    and serialiseAnonObject (obj, d) =
        let val triples = Matcher.match (#matcher d, (SOME obj, NONE, NONE))
        in
            TextIO.output (#stream d, "[\n");
            let val d' =
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
                TextIO.output (#stream d', "\n");
                writeIndent d';
                TextIO.output (#stream d', "]");
                d'
            end
        end

    and serialiseStringLiteral (lit, d) =
        let val long = shouldUseLongString lit
            val quote = if long then "\"\"\"" else "\""
            fun serialise s = TextIO.output (#stream d, s)
        in
            serialise quote;
            serialise (Encode.encodeStringExcept
                           (if shouldUseLongString lit
                            then (TurtleCodepoints.longStringDoubleEscaped,
                                  Encode.backslashEncode)
                            else (TurtleCodepoints.shortStringDoubleExcluded,
                                  Encode.asciiEncode))
                           (#value lit));
            serialise quote;
            if #lang lit = "" then ()
            else serialise ("@" ^ (#lang lit));
            if Iri.isEmpty (#dtype lit) then ()
            else serialise ("^^" ^ stringOfAbbrIri (#dtype lit, d));
            d
        end

    and serialiseBooleanLiteral (lit, d) =
        case #value lit of
            "true" => (TextIO.output (#stream d, "true"); d)
          | "false" => (TextIO.output (#stream d, "false"); d)
          | _ => serialiseStringLiteral (lit, d)

    and serialiseIntegerLiteral (lit, d) =
        if List.find (fn c => not (Char.isDigit c))
                     (String.explode (#value lit)) = NONE
        then (TextIO.output (#stream d, #value lit); d)
        else serialiseStringLiteral (lit, d)
                                          
    and serialiseAbbreviated (IRI iri, d : ser_data) =
        (TextIO.output (#stream d, stringOfAbbrIri (iri, d)); d)
      | serialiseAbbreviated (BLANK n, d : ser_data) =
        (TextIO.output (#stream d, "_:blank" ^ (Int.toString n)); d)
      | serialiseAbbreviated (LITERAL (lit as { dtype, ... }), d : ser_data) =
        if dtype = RdfStandardIRIs.iriTypeBoolean
        then serialiseBooleanLiteral (lit, d)
        else if dtype = RdfStandardIRIs.iriTypeInteger
        then serialiseIntegerLiteral (lit, d)
        (*!!! + double, decimal *)
        else serialiseStringLiteral (lit, d)

    and serialiseObject (obj, d, pr : ser_props) : ser_data =
        if (#isAnon pr)
        then (TextIO.output (#stream d, " "); serialiseAnonObject (obj, d))
        else (TextIO.output (#stream d, " "); serialiseAbbreviated (obj, d))

    and serialiseCollection (obj, d, pr) =
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
                serialiseObject (obj, d, pr)
            else
                let val _ = TextIO.output (#stream d, " (");
                    val d = 
                        foldl (fn (t as (_, pred, obj) : RdfTriple.triple, d) =>
                                  let val d = 
                                          { stream = #stream d,
                                            base = #base d,
                                            subject = #subject d,
                                            predicate = #predicate d,
                                            indent = #indent d,
                                            written = Triples.add (#written d, t),
                                            prefixes = #prefixes d,
                                            matcher = #matcher d}
                                  in
                                      if pred = IRI RdfStandardIRIs.iriRdfFirst
                                      then
                                          (TextIO.output (#stream d, " ");
                                           serialiseAbbreviated (obj, d))
                                      else d
                                  end)
                              d triples
                in
                    TextIO.output (#stream d, " )");
                    d
                end
        end
                 
    and serialiseObjectOrCollection (obj, d, pr : ser_props) =
        if (#isColl pr)
        then serialiseCollection (obj, d, pr)
        else serialiseObject (obj, d, pr)

    and indented n (d : ser_data) =
        { stream = #stream d,
          base = #base d,
          subject = #subject d,
          predicate = #predicate d,
          indent = n + #indent d,
          written = #written d,
          prefixes = #prefixes d,
          matcher = #matcher d }
                              
    and serialiseSubjectPredicate (subj, pred, d : ser_data, pr) =
        case #subject d of
            NONE =>
            (* first triple in graph *)
            (serialiseNodes (d, pr) [subj, pred];
             indented 1 d)
          | SOME currentSubj =>
            if currentSubj = subj then
                case #predicate d of
                    NONE =>
                    (* first triple in bnode [] syntax *)
                    serialiseNodes (d, pr) [pred]
                  | SOME currentPred =>
                    if currentPred = pred then
                        (TextIO.output (#stream d, ",");
                         d)
                    else
                        (TextIO.output (#stream d, " ;\n");
                         serialiseNodes (d, pr) [pred])
            else
                (TextIO.output (#stream d, " .\n\n");
                 indented 1 (serialiseNodes (indented (~1) d, pr) [subj, pred]))

    and serialiseTripleParts ((subj, pred, obj), d : ser_data, pr) =
        let val d = serialiseSubjectPredicate (subj, pred, d, pr)
            val d = serialiseObjectOrCollection (obj, d, pr)
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
            fun hasBlankObject (_, _, BLANK _) = true
              | hasBlankObject _ = false

            fun isBlankObjectUnique (d : ser_data, t) =
                Matcher.match (#matcher d, (NONE, NONE, SOME (#3 t))) = [t]

            fun wasBlankObjectWritten (d : ser_data, (_,_,obj)) =
                List.exists
                    (fn x => wasWritten (x, d))
                    (Matcher.match (#matcher d, (SOME obj, NONE, NONE)))

            fun isBlankObjectUnwritten args = not (wasBlankObjectWritten args)

            val isAnon = hasBlankObject triple andalso
                          isBlankObjectUnique (d, triple) andalso
                          isBlankObjectUnwritten (d, triple)

            val isColl = isAnon andalso
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
                     { isAnon = isAnon,
                       isColl = isColl })
        end
                        
    and serialiseTriples data triples =
        foldl (fn (t, d) => serialiseTripleMaybe (t, d)) data triples
        
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

                                                   
