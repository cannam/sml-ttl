
functor RdfSerialiserFn (S: RDF_STREAM_SERIALISER) : RDF_SERIALISER = struct

    type prefix = S.prefix
    type triple = S.triple

    fun serialise_to_stream stream (prefixes, triples) =
        ignore
            (foldl (fn (t, s) => S.serialise (s, S.TRIPLE t))
                   (foldl (fn (p, s) => S.serialise (s, S.PREFIX p))
                          (S.create_for_stream stream)
                          prefixes)
                   triples)

    fun serialise_to_file filename data =
        let val stream = TextIO.openOut filename
            val _ = serialise_to_stream stream
        in
            TextIO.closeOut stream
        end
    
end
                                                                          
