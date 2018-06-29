
(** Turn an incremental parser into a one-shot parser *)

functor RdfParserFn (I: RDF_INCREMENTAL_PARSER) : RDF_PARSER = struct
    
    type prefix = Prefix.prefix
    type triple = RdfTriple.triple
    type base_iri = BaseIri.t
                      
    datatype parsed =
             PARSE_ERROR of string |
             PARSED of {
                 base : base_iri,
                 prefixes : prefix list,
                 triples : triple list
             }

    fun parse (base_iri, stream) : parsed =
        let fun parse' acc f =
                case f () of
                    I.END_OF_STREAM => PARSED acc
                  | I.PARSE_ERROR err => PARSE_ERROR err
                  | I.PARSE_OUTPUT ({ base, prefixes, triples }, f') =>
                    parse' {
                        base = case base of NONE => #base acc 
                                          | _ => base,
                        prefixes = List.revAppend(prefixes, #prefixes acc),
                        triples = List.revAppend(triples, #triples acc)
                    } f'
        in
            case parse' { base = NONE, prefixes = [], triples = [] }
                        (fn () => I.parse (base_iri, stream)) of
                PARSED { base,
                         prefixes,
                         triples } =>
                PARSED { base = base,
                         prefixes = rev prefixes,
                         triples = rev triples }
              | err => err
        end

end
                                          
