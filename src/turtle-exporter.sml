
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

    fun local_encode str = str (*!!! *)
              
    fun serialise_nodes (d, pr) nodes =
        raise Fail "serialise_nodes not yet implemented"

    fun serialise_anon_object (obj, d) =
        raise Fail "serialise_anon_object not implemented yet"

    fun string_of_abbr_iri (iri, d : ser_data) =
        case Store.abbreviate (#store d, iri) of
            SOME abbr => local_encode abbr
          | NONE => NTriplesEncoders.string_of_node (IRI iri)
              
    fun serialise_abbreviated (IRI iri, d : ser_data) =
        TextIO.output (#stream d, string_of_abbr_iri (iri, d))
      | serialise_abbreviated (node, d) = 
        TextIO.output (#stream d, NTriplesEncoders.string_of_node node)

    fun serialise_object (obj, d, pr : ser_props) : ser_data =
        if (#is_anon pr)
        then (TextIO.output (#stream d, " "); serialise_anon_object (obj, d); d)
        else (TextIO.output (#stream d, " "); serialise_abbreviated (obj, d); d)

    fun serialise_collection (obj, d, pr) =
        raise Fail "serialise_collection not implemented yet"
                 
    fun serialise_object_or_collection (obj, d, pr : ser_props) =
        if (#is_coll pr)
        then serialise_collection (obj, d, pr)
        else serialise_object (obj, d, pr)

    fun serialise_subject_predicate (subj, pred, d : ser_data, pr) =
        case #subject d of
            NONE => serialise_nodes (d, pr) [subj, pred] (* first triple in graph *)
          | SOME current_subj =>
            if current_subj = subj then
                case #predicate d of
                    (*!!! indent *)
                    NONE => serialise_nodes (d, pr) [pred] (* first triple in bnode [] syntax *)
                  | SOME current_pred =>
                    if current_pred = pred then
                        (TextIO.output (#stream d, ",");
                         d)
                    else
                        (TextIO.output (#stream d, " ;\n");
                         serialise_nodes (d, pr) [pred])
            else
                (TextIO.output (#stream d, " .\n\n");
                 serialise_nodes (d, pr) [subj, pred])

    fun serialise_triple_parts ((subj, pred, obj), d : ser_data, pr) =
        let val d = serialise_subject_predicate (subj, pred, d, pr)
            val d = serialise_object (obj, d, pr)
        in
            { stream = #stream d,
              subject = SOME subj,
              predicate = SOME pred,
              indent = #indent d,
              written = #written d,
              store = #store d }
        end

    fun serialise_triple_maybe (triple, d) =
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
                        
    fun serialise_triples data =
        Store.foldl (fn (t, d) => serialise_triple_maybe (t, d)) data (#store data)
        
    fun serialise_prefixes stream prefixes =
        foldl (fn ((pfx, iri), t) =>
                  (TextIO.output (t, ("@prefix " ^ pfx ^ ": <" ^ iri ^ "> .\n"));
                   t))
              stream prefixes

    fun save_to_stream store stream =
        let val stream = serialise_prefixes stream (Store.enumerate_prefixes store)
        in
            ignore (serialise_triples
                        { stream = stream,
                          subject = NONE,
                          predicate = NONE,
                          indent = 0,
                          written = Triples.empty,
                          store = store })
        end

    fun save_to_file store filename =
        let val stream = TextIO.openOut filename
            val _ = save_to_stream store stream
        in
            TextIO.closeOut stream
        end
            
end

                                                   
