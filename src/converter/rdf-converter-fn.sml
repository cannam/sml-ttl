
signature RDF_INCREMENTAL_CONVERTER_ARG = sig
    structure Parser : RDF_INCREMENTAL_PARSER
    structure Serialiser : RDF_INCREMENTAL_SERIALISER
end

functor RdfIncrementalConverterFn (A: RDF_INCREMENTAL_CONVERTER_ARG)
        : RDF_CONVERTER = struct
    
    structure P = A.Parser
    structure S = A.Serialiser

    type triple = RdfTriple.triple
    type base_iri = BaseIri.t

    datatype result =
             CONVERSION_ERROR of string |
             OK
                 
    fun convert (in_base, instream) (out_base, outstream) =
        let fun convert' s f =
                case f () of
                    P.END_OF_STREAM => (S.finish s; OK)
                  | P.PARSE_ERROR err => (S.finish s; CONVERSION_ERROR err)
                  | P.PARSE_OUTPUT ({ prefixes, triples }, f') =>
                    convert' (S.serialise (s, triples)) f'
        in
            (* incremental serialiser doesn't use out_base iri *)
            convert' (S.new outstream)
                     (fn () => P.parse (in_base, instream))
        end

end

