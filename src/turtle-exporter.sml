
(* This is an exporter, rather than just a serialiser like the
   NTriples one, because it needs to be backed by a store *)

structure TurtleExporter : STORE_EXPORTER = struct

    structure Store = Store

    datatype patnode = datatype Store.patnode

    open RdfTriple

    fun sorted_by_subject triples = (* with iri-subject triples before blanks *)
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
        subject   : RdfNode.node option,
        predicate : RdfNode.node option,
        indent    : int,
        written   : Triples.set,
        store     : Store.t
    }

    type ser_props = {
        is_anon   : bool,
        is_coll   : bool
    }        

    fun was_written (triple, d : ser_data) = Triples.member (#written d, triple)

    fun encode_local str =
        Encode.encode_string_except (TurtleCodepoints.pname_local_escapable,
                                     Encode.backslash_encode)
                                    str

    fun encode_iri iri = Iri.toString iri
            
    fun string_of_abbr_iri (iri, d : ser_data) =
	if iri = RdfStandardIRIs.iri_rdf_type then "a"
	else case Store.abbreviate (#store d, iri) of
                 SOME (ns, rest) => ns ^ ":" ^ encode_local rest
               | NONE => "<" ^ (encode_iri iri) ^ ">"

    fun string_for_indent indent =
        if indent < 0 then ""
        else String.concatWith "" (List.tabulate (indent * 4, fn _ => " "))

    fun write_indent (d : ser_data) =
        (TextIO.output (#stream d, string_for_indent (#indent d));
         d)
            
    fun should_use_long_string (lit : RdfNode.literal) =
        String.size (#value lit) > 80 (* arbitrary, avoid scanning v long strings *)
        orelse
        List.exists
            (fn w => CodepointSet.contains
                         TurtleCodepoints.short_string_double_excluded
                         w)
            (WdString.explodeUtf8 (#value lit))
                          
    fun serialise_nodes (d, pr) nodes =
        case foldl (fn (n, (d, sep)) =>
                       (TextIO.output (#stream d, sep);
                        serialise_abbreviated (n, d);
                        (d, " ")))
                   ((write_indent d), "")
                   nodes
         of (d, _) => d

    and serialise_anon_object (obj, d) =
        let val triples = Store.match (#store d, (KNOWN obj, WILDCARD, WILDCARD))
        in
            TextIO.output (#stream d, "[\n");
            let val d' =
                    indented (~1)
                             (serialise_triples {
                                   stream = #stream d,
                                   subject = SOME obj,
                                   predicate = NONE,
                                   indent = 1 + #indent d,
                                   written = #written d,
                                   store = #store d
                               } triples)
            in
                TextIO.output (#stream d', "\n");
                write_indent d';
                TextIO.output (#stream d', "]");
                d'
            end
        end

    and serialise_string_literal (lit, d) =
        let val long = should_use_long_string lit
            val quote = if long then "\"\"\"" else "\""
            fun serialise s = TextIO.output (#stream d, s)
        in
            serialise quote;
            serialise (Encode.encode_string_except
                           (if should_use_long_string lit
                            then TurtleCodepoints.long_string_double_excluded
                            else TurtleCodepoints.short_string_double_excluded,
                            Encode.backslash_encode)
                           (#value lit));
            serialise quote;
            if #lang lit = "" then ()
            else serialise ("@" ^ (#lang lit));
            if Iri.is_empty (#dtype lit) then ()
            else serialise ("^^" ^ string_of_abbr_iri (#dtype lit, d));
            d
        end

    and serialise_boolean_literal (lit, d) =
        case #value lit of
            "true" => (TextIO.output (#stream d, "true"); d)
          | "false" => (TextIO.output (#stream d, "false"); d)
          | _ => serialise_string_literal (lit, d)

    and serialise_integer_literal (lit, d) =
        if List.find (fn c => not (Char.isDigit c))
                     (String.explode (#value lit)) = NONE
        then (TextIO.output (#stream d, #value lit); d)
        else serialise_string_literal (lit, d)
                                          
    and serialise_abbreviated (IRI iri, d : ser_data) =
        (TextIO.output (#stream d, string_of_abbr_iri (iri, d)); d)
      | serialise_abbreviated (BLANK n, d : ser_data) =
        (TextIO.output (#stream d, "_:blank" ^ (Int.toString n)); d)
      | serialise_abbreviated (LITERAL (lit as { dtype, ... }), d : ser_data) =
        if dtype = RdfStandardIRIs.iri_type_boolean
        then serialise_boolean_literal (lit, d)
        else if dtype = RdfStandardIRIs.iri_type_integer
        then serialise_integer_literal (lit, d)
        (*!!! + double, decimal *)
        else serialise_string_literal (lit, d)

    and serialise_object (obj, d, pr : ser_props) : ser_data =
        if (#is_anon pr)
        then (TextIO.output (#stream d, " "); serialise_anon_object (obj, d))
        else (TextIO.output (#stream d, " "); serialise_abbreviated (obj, d))

    and serialise_collection (obj, d, pr) =
        let val triples = StoreCollection.triples_of_collection (#store d, obj)
            val any_written = List.exists (fn t => was_written (t, d)) triples
        in
            if any_written
            then
                (* Can't write as a collection, because one or more of
                   its metanodes have already been written. This could be
                   because the head of the collection is a subject node
                   rather than an object. *)
                serialise_object (obj, d, pr)
            else
                let val _ = TextIO.output (#stream d, " (");
                    val d = 
                        foldl (fn (t as (_, pred, obj) : triple, d) =>
                                  let val d = 
                                          { stream = #stream d,
                                            subject = #subject d,
                                            predicate = #predicate d,
                                            indent = #indent d,
                                            written = Triples.add (#written d, t),
                                            store = #store d }
                                  in
                                      if pred = IRI RdfStandardIRIs.iri_rdf_first
                                      then
                                          (TextIO.output (#stream d, " ");
                                           serialise_abbreviated (obj, d))
                                      else d
                                  end)
                              d triples
                in
                    TextIO.output (#stream d, " )");
                    d
                end
        end
                 
    and serialise_object_or_collection (obj, d, pr : ser_props) =
        if (#is_coll pr)
        then serialise_collection (obj, d, pr)
        else serialise_object (obj, d, pr)

    and indented n (d : ser_data) =
        { stream = #stream d,
          subject = #subject d,
          predicate = #predicate d,
          indent = n + #indent d,
          written = #written d,
          store = #store d }
                              
    and serialise_subject_predicate (subj, pred, d : ser_data, pr) =
        case #subject d of
            NONE =>
            (* first triple in graph *)
            (serialise_nodes (d, pr) [subj, pred];
             indented 1 d)
          | SOME current_subj =>
            if current_subj = subj then
                case #predicate d of
                    NONE =>
                    (* first triple in bnode [] syntax *)
                    serialise_nodes (d, pr) [pred]
                  | SOME current_pred =>
                    if current_pred = pred then
                        (TextIO.output (#stream d, ",");
                         d)
                    else
                        (TextIO.output (#stream d, " ;\n");
                         serialise_nodes (d, pr) [pred])
            else
                (TextIO.output (#stream d, " .\n\n");
                 indented 1 (serialise_nodes (indented (~1) d, pr) [subj, pred]))

    and serialise_triple_parts ((subj, pred, obj), d : ser_data, pr) =
        let val d = serialise_subject_predicate (subj, pred, d, pr)
            val d = serialise_object_or_collection (obj, d, pr)
        in
            { stream = #stream d,
              subject = SOME subj,
              predicate = SOME pred,
              indent = #indent d,
              written = #written d,
              store = #store d }
        end

    and serialise_triple_maybe (triple, d) =
        let
            fun has_blank_object (_, _, BLANK _) = true
              | has_blank_object _ = false

            fun is_blank_object_unique (d : ser_data, t) =
                Store.match (#store d, (WILDCARD, WILDCARD, KNOWN (#3 t))) = [t]

            fun was_blank_object_written (d : ser_data, (_,_,obj)) =
                List.exists
                    (fn x => was_written (x, d))
                    (Store.match (#store d, (KNOWN obj, WILDCARD, WILDCARD)))

            fun is_blank_object_unwritten args = not (was_blank_object_written args)

            val is_anon = has_blank_object triple andalso
                          is_blank_object_unique (d, triple) andalso
                          is_blank_object_unwritten (d, triple)

            val is_coll = is_anon andalso
                          StoreCollection.is_collection_node (#store d, #3 triple)
                                                             
        in
            if was_written (triple, d) then d
            else
                serialise_triple_parts
                    (triple,
                     { stream = #stream d,
                       subject = #subject d,
                       predicate = #predicate d,
                       indent = #indent d,
                       written = Triples.add (#written d, triple),
                       store = #store d },
                     { is_anon = is_anon,
                       is_coll = is_coll })
        end
                        
    and serialise_triples data triples =
        foldl (fn (t, d) => serialise_triple_maybe (t, d)) data triples
                        
    and serialise_store_triples data =
        Store.foldl (fn (t, d) => serialise_triple_maybe (t, d)) data (#store data)
        
    and serialise_prefixes stream prefixes =
        foldl (fn ((pfx, iri), t) =>
                  (TextIO.output (t, ("@prefix " ^ pfx ^ ": <" ^ iri ^ "> .\n"));
                   t))
              stream prefixes

    fun save_to_stream store stream =
        let val stream = serialise_prefixes stream (Store.enumerate_prefixes store)
            val _ = TextIO.output (stream, "\n")
            val d = serialise_store_triples
                        { stream = stream,
                          subject = NONE,
                          predicate = NONE,
                          indent = 0,
                          written = Triples.empty,
                          store = store }
        in
            if not (Triples.isEmpty (#written d))
            then TextIO.output (stream, " .")
            else ();
            TextIO.output (stream, "\n")
        end

    fun save_to_file store filename =
        let val stream = TextIO.openOut filename
            val _ = save_to_stream store stream
        in
            TextIO.closeOut stream
        end
            
end

                                                   
