
structure StoreLoadBase : STORE_LOAD_BASE = struct

    datatype result = LOAD_ERROR of string | OK of Store.t
    type base_iri = string

end
                                            
functor StoreIncrementalLoaderFn (P: RDF_INCREMENTAL_PARSER)
        : STORE_LOADER where type store = Store.t = struct

    type store = Store.t

    open StoreLoadBase

    fun load_stream store iri stream : result =
	let fun parse' acc f =
		case f () of
		    P.END_OF_STREAM => OK acc
		  | P.PARSE_ERROR err => LOAD_ERROR err
		  | P.PARSE_OUTPUT ({ prefixes, triples }, f') =>
		    parse'
			(foldl (fn (triple, s) => Store.add (s, triple))
			       (foldl (fn ((pfx, exp), s) =>
					  Store.add_prefix (s, pfx, exp))
				      acc prefixes)
			       triples)
			f'
	in
	    parse' Store.empty (fn () => P.parse iri stream)
	end
								  
    fun load_string store iri string =
        let val stream = TextIO.openString string
            val result = load_stream store iri stream
        in
            TextIO.closeIn stream;
            result
        end

    fun load_file store iri filename =
        let val stream = TextIO.openIn filename
            val result = load_stream store iri stream
        in
            TextIO.closeIn stream;
            result
        end

    val load_stream_as_new_store = load_stream Store.empty
    val load_string_as_new_store = load_string Store.empty
    val load_file_as_new_store = load_file Store.empty
			
end

structure TurtleLoader = StoreIncrementalLoaderFn(TurtleIncrementalParser)
                                            
structure StoreFileLoader :> STORE_FILE_LOADER where type store = Store.t = struct

    type store = Store.t

    open StoreLoadBase

    fun load_file store iri filename =
        let open FileType
            val loader =
                case type_of filename of
                    TURTLE => TurtleLoader.load_file 
                  | NTRIPLES => TurtleLoader.load_file
                  | _ => raise Fail "Unknown or unsupported file extension"
        in
            loader store iri filename
        end

    val load_file_as_new_store = load_file Store.empty

end
