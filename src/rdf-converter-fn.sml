
signature RDF_CONVERTER_STREAM_ARG = sig
    structure Parser : RDF_STREAM_PARSER
    structure Serialiser : RDF_STREAM_SERIALISER
end

functor RdfConverterStreamFn (A: RDF_CONVERTER_STREAM_ARG) :> RDF_CONVERTER = struct
    structure P = A.Parser
    structure S = A.Serialiser

    type prefix = RdfTriple.prefix
    type triple = RdfTriple.triple

    type base_iri = string

    datatype result = CONVERSION_ERROR of string |
                      CONVERTED
                        
    fun convert base_iri instream outstream =
        let fun serialise_chunk serialiser prefixes triples =
                foldl (fn (t, s) => S.serialise (s, S.TRIPLE t))
                      (foldl (fn (p, s) => S.serialise (s, S.PREFIX p))
                             serialiser
                             prefixes)
                      triples
            fun parse' acc f =
                case f () of
                    P.END_OF_STREAM => CONVERTED
                  | P.PARSE_ERROR err => CONVERSION_ERROR err
                  | P.PARSE_OUTPUT ({ prefixes, triples }, f') =>
                    parse' (serialise_chunk acc prefixes triples) f'
        in
            parse' (S.new outstream) (fn () => P.parse base_iri instream)
        end

end

