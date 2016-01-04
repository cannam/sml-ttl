
structure TurtleExporter : STORE_EXPORTER = struct

    structure Store = Store

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
        written   : Triples.set
    }

    type ser_type = {
        is_anon   : bool,
        is_coll   : bool
    }        

    fun serialise_triple_parts (triple, d) = (* !!! *)
        raise Fail "serialise_triple_parts: Not implemented yet"
                        
    fun serialise_triple_maybe (triple, d, store) =
        if Triples.member (#written d, triple) then d
        else
            serialise_triple_parts
                (triple,
                 { stream = #stream d,
                   subject = #subject d,
                   predicate = #predicate d,
                   indent = #indent d,
                   written = Triples.add (#written d, triple) }
                     (*!!! is anon, coll *)
                 )
                        
    fun serialise_triples data store =
        Store.foldl (fn (t, d) => serialise_triple_maybe (t, d, store)) data store 
        
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
                          written = Triples.empty }
                        store)
        end

    fun save_to_file store filename =
        let val stream = TextIO.openOut filename
            val _ = save_to_stream store stream
        in
            TextIO.closeOut stream
        end
            
end

                                                   
