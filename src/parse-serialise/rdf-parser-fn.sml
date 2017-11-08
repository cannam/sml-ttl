
functor RdfParserFn (P: RDF_INCREMENTAL_PARSER) : RDF_PARSER = struct

    (* Turn an incremental parser into a one-shot parser *)

    open RdfParserBase

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
                                          
