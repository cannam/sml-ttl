
structure RdfConverterBase = struct
    datatype result = CONVERSION_ERROR of string |
                      CONVERTED
end

signature RDF_INCREMENTAL_CONVERTER_ARG = sig
    structure Parser : RDF_INCREMENTAL_PARSER
    structure Serialiser : RDF_INCREMENTAL_SERIALISER
end

functor RdfIncrementalConverterFn (A: RDF_INCREMENTAL_CONVERTER_ARG)
        : RDF_CONVERTER = struct
    
    structure P = A.Parser
    structure S = A.Serialiser

    type triple = RdfTriple.triple

    type base_iri = string

    open RdfConverterBase
                        
    fun convert base_iri instream outstream =
        let fun convert' s f =
                case f () of
                    P.END_OF_STREAM => (S.finish s; CONVERTED)
                  | P.PARSE_ERROR err => (S.finish s; CONVERSION_ERROR err)
                  | P.PARSE_OUTPUT ({ prefixes, triples }, f') =>
                    convert' (S.serialise (s, triples)) f'
        in
            convert' (S.new outstream) (fn () => P.parse base_iri instream)
        end

end

