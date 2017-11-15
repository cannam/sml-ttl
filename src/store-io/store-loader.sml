
structure StoreLoadBase : STORE_LOAD_BASE = struct

    type base_iri = BaseIri.t
    type store = Store.t

    datatype result =
             FORMAT_NOT_SUPPORTED |
             SYSTEM_ERROR of string |
             PARSE_ERROR of string |
             OK of store
                       
end
                                            
functor StoreIncrementalLoaderFn (P: RDF_INCREMENTAL_PARSER)
        :> STORE_LOADER
               where type store = Store.t
               where type result = StoreLoadBase.result
= struct

    open StoreLoadBase

    fun load_stream store (base_iri, stream) : result =
	let fun parse' acc f =
		case f () of
		    P.END_OF_STREAM => OK acc
		  | P.PARSE_ERROR err => PARSE_ERROR err
		  | P.PARSE_OUTPUT ({ prefixes, triples }, f') =>
		    parse'
			(foldl (fn (triple, s) => Store.add (s, triple))
			       (foldl (fn ((pfx, exp), s) =>
					  Store.add_prefix (s, pfx, exp))
				      acc prefixes)
			       triples)
			f'
	in
	    parse' Store.empty (fn () => P.parse (base_iri, stream))
	end
								  
    fun load_string' store (base_iri, string) =
        let val stream = TextIO.openString string
            val result = load_stream store (base_iri, stream)
        in
            TextIO.closeIn stream;
            result
        end

    fun load_string store (base_iri, string) =
        load_string' store (base_iri, string)
        handle ex => SYSTEM_ERROR (exnMessage ex)

    fun load_file' store (base_iri, filename) =
        let val stream = TextIO.openIn filename
            val result = load_stream store (base_iri, stream)
        in
            TextIO.closeIn stream;
            result
        end

    fun load_file store (base_iri, filename) =
        load_file' store (base_iri, filename)
        handle ex => SYSTEM_ERROR (exnMessage ex)
            
    val load_stream_as_new_store = load_stream Store.empty
    val load_string_as_new_store = load_string Store.empty
    val load_file_as_new_store = load_file Store.empty
			
end

structure TurtleLoader = StoreIncrementalLoaderFn(TurtleIncrementalParser)
                                            
structure StoreFileLoader :> STORE_FILE_LOADER
               where type store = Store.t
               where type result = StoreLoadBase.result
= struct

    open StoreLoadBase

    exception Unsupported
             
    fun load_file store (base_iri, filename) =
        let open FileType
            val loader =
                case format_of filename of
                    TURTLE => TurtleLoader.load_file 
                  | NTRIPLES => TurtleLoader.load_file
                  | _ => raise Unsupported
        in
            loader store (base_iri, filename)
            handle Unsupported => FORMAT_NOT_SUPPORTED
            handle ex => SYSTEM_ERROR (exnMessage ex)
        end

    val load_file_as_new_store = load_file Store.empty

    val formats_supported = [FileType.TURTLE, FileType.NTRIPLES]
                                           
    val extensions_supported =
        List.concat (map FileType.extensions_for_format formats_supported)
                                           
end
