
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

    datatype result =
             CONVERSION_ERROR of string |
             CONVERTED
                 
    fun convert (in_base_iri, instream) (out_base_iri, outstream) =
        let fun convert' s f =
                case f () of
                    P.END_OF_STREAM => (S.finish s; CONVERTED)
                  | P.PARSE_ERROR err => (S.finish s; CONVERSION_ERROR err)
                  | P.PARSE_OUTPUT ({ prefixes, triples }, f') =>
                    convert' (S.serialise (s, triples)) f'
        in
            convert' (S.new outstream) (*!!! todo: out_base_iri *)
                     (fn () => P.parse in_base_iri instream)
        end

end

