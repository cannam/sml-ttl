
structure StoreLoadBase : STORE_LOAD_BASE = struct

    type base_iri = BaseIri.t
    type store = Store.t

    datatype result =
             FORMAT_NOT_SUPPORTED |
             SYSTEM_ERROR of string |
             PARSE_ERROR of string |
             OK of base_iri * store
                       
end
                                            
functor StoreIncrementalLoaderFn (P: RDF_INCREMENTAL_PARSER)
        :> STORE_LOADER
               where type store = Store.t
               where type result = StoreLoadBase.result
= struct

    open StoreLoadBase

    fun loadStream store (base_iri, stream) : result =
	let fun parse' acc f =
		case f () of
		    P.END_OF_STREAM => OK acc
		  | P.PARSE_ERROR err => PARSE_ERROR err
		  | P.PARSE_OUTPUT ({ base, prefixes, triples }, f') =>
                    let val (prior_base, store) = acc
                        val base = case base of NONE => prior_base 
                                              | _ => base
                    in
		        parse'
			    (base,
                             foldl (fn (triple, s) => Store.add (s, triple))
			           (foldl (fn ((abbr, exp), s) =>
					      Store.addPrefix (s, (abbr, exp)))
				          store prefixes)
			           triples)
			    f'
                    end
	in
	    parse' (base_iri, Store.empty)
                   (fn () => P.parse (base_iri, stream))
	end
								  
    fun loadString' store (base_iri, string) =
        let val stream = CodepointIO.openString string
            val result = loadStream store (base_iri, stream)
        in
            CodepointIO.closeIn stream;
            result
        end

    fun loadString store (base_iri, string) =
        loadString' store (base_iri, string)
        handle ex => SYSTEM_ERROR (exnMessage ex)

    fun loadFile' store (base_iri, filename) =
        let val stream = CodepointIO.openIn filename
            val result = loadStream store (base_iri, stream)
        in
            CodepointIO.closeIn stream;
            result
        end

    fun loadFile store (base_iri, filename) =
        loadFile' store (base_iri, filename)
        handle ex => SYSTEM_ERROR (exnMessage ex)
            
    val loadStreamAsNewStore = loadStream Store.empty
    val loadStringAsNewStore = loadString Store.empty
    val loadFileAsNewStore = loadFile Store.empty
			
end

structure TurtleLoader = StoreIncrementalLoaderFn(TurtleIncrementalParser)
                                            
structure StoreFileLoader :> STORE_FILE_LOADER
               where type store = Store.t
               where type result = StoreLoadBase.result
= struct

    open StoreLoadBase

    exception Unsupported
             
    fun loadFile store (base_iri, filename) =
        let open FileType
            val loader =
                case formatOf filename of
                    TURTLE => TurtleLoader.loadFile 
                  | NTRIPLES => TurtleLoader.loadFile
                  | _ => raise Unsupported
        in
            loader store (base_iri, filename)
            handle Unsupported => FORMAT_NOT_SUPPORTED
            handle ex => SYSTEM_ERROR (exnMessage ex)
        end

    val loadFileAsNewStore = loadFile Store.empty

    val formatsSupported = [FileType.TURTLE, FileType.NTRIPLES]
                                           
    val extensionsSupported =
        List.concat (map FileType.extensionsForFormat formatsSupported)
                                           
end
