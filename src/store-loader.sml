
structure StoreLoadBase : STORE_LOAD_BASE = struct

    datatype result = LOAD_ERROR of string | OK of Store.t
    type base_iri = string

end
                                            
functor StoreStreamLoaderFn (P: RDF_STREAM_PARSER) : STORE_LOADER where type store = Store.t = struct

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

structure TurtleLoader = StoreStreamLoaderFn(TurtleStreamParser)
                                            
structure StoreFileLoader :> STORE_FILE_LOADER where type store = Store.t = struct

    type store = Store.t

    open StoreLoadBase

    fun load_file store iri filename =
        let val loader =
                case FileExtension.extension filename of
                    "ttl" => TurtleLoader.load_file 
                  | "n3" => TurtleLoader.load_file
                  | "ntriples" => TurtleLoader.load_file
                  | "nt" => TurtleLoader.load_file
                  | other => raise Fail ("Unknown or unsupported file extension \""
                                         ^ other ^ "\"")
        in
            loader store iri filename
        end

    val load_file_as_new_store = load_file Store.empty

end
