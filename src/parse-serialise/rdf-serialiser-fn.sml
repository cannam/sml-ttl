
functor RdfSerialiserFn (S: RDF_STREAM_SERIALISER) : RDF_SERIALISER = struct

    type prefix = S.prefix
    type triple = S.triple

    fun serialise (stream, prefixes, triples) =
        ignore
            (foldl (fn (t, s) => S.serialise (s, S.TRIPLE t))
                   (foldl (fn (p, s) => S.serialise (s, S.PREFIX p))
                          (S.new stream)
                          prefixes)
                   triples)
    
end
                                                                          
