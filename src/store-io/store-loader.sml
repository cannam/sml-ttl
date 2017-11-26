
structure StoreLoadBase : STORE_LOAD_BASE = struct

    type baseIri = BaseIri.t
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

    fun loadStream store (baseIri, stream) : result =
	let fun parse' acc f =
		case f () of
		    P.END_OF_STREAM => OK acc
		  | P.PARSE_ERROR err => PARSE_ERROR err
		  | P.PARSE_OUTPUT ({ prefixes, triples }, f') =>
		    parse'
			(foldl (fn (triple, s) => Store.add (s, triple))
			       (foldl (fn ((abbr, exp), s) =>
					  Store.addPrefix (s, (abbr, exp)))
				      acc prefixes)
			       triples)
			f'
	in
	    parse' Store.empty (fn () => P.parse (baseIri, stream))
	end
								  
    fun loadString' store (baseIri, string) =
        let val stream = TextIO.openString string
            val result = loadStream store (baseIri, stream)
        in
            TextIO.closeIn stream;
            result
        end

    fun loadString store (baseIri, string) =
        loadString' store (baseIri, string)
        handle ex => SYSTEM_ERROR (exnMessage ex)

    fun loadFile' store (baseIri, filename) =
        let val stream = TextIO.openIn filename
            val result = loadStream store (baseIri, stream)
        in
            TextIO.closeIn stream;
            result
        end

    fun loadFile store (baseIri, filename) =
        loadFile' store (baseIri, filename)
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
             
    fun loadFile store (baseIri, filename) =
        let open FileType
            val loader =
                case formatOf filename of
                    TURTLE => TurtleLoader.loadFile 
                  | NTRIPLES => TurtleLoader.loadFile
                  | _ => raise Unsupported
        in
            loader store (baseIri, filename)
            handle Unsupported => FORMAT_NOT_SUPPORTED
            handle ex => SYSTEM_ERROR (exnMessage ex)
        end

    val loadFileAsNewStore = loadFile Store.empty

    val formatsSupported = [FileType.TURTLE, FileType.NTRIPLES]
                                           
    val extensionsSupported =
        List.concat (map FileType.extensionsForFormat formatsSupported)
                                           
end
