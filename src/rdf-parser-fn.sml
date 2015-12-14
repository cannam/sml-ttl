
functor RdfParserFn (P: RDF_STREAM_PARSER) : RDF_PARSER = struct

    type prefix = P.prefix
    type triple = P.triple
    type base_iri = P.base_iri

    datatype parsed =
             PARSE_ERROR of string |
             PARSED of {
                 prefixes : prefix list,
                 triples : triple list
             }

    fun parse iri stream : parsed =
        let fun parse' acc f =
                case f () of
                    P.END_OF_STREAM => PARSED acc
                  | P.PARSE_ERROR err => PARSE_ERROR err
                  | P.PARSE_OUTPUT ({ prefixes, triples }, f') =>
                    parse' {
                        prefixes = List.revAppend(prefixes, #prefixes acc),
                        triples = List.revAppend(triples, #triples acc)
                    } f'
        in
            case parse' { prefixes = [], triples = [] }
                        (fn () => P.parse iri stream) of
                PARSED { prefixes, triples } => PARSED { prefixes = rev prefixes,
                                                         triples = rev triples }
              | PARSE_ERROR e => PARSE_ERROR e
        end

end
                                          
