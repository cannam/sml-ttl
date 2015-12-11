
signature RDF_STREAM_SERIALISER = sig

    datatype entry =
	     PREFIX of RdfTriple.prefix |
	     TRIPLE of RdfTriple.triple
	     
    type t

    val create_for_stream : TextIO.outstream -> t
    val serialise : t * entry -> t

end

signature RDF_SERIALISER = sig

    type store

    val serialise_to_stream : TextIO.outstream -> store -> unit
    val serialise_to_file : string -> store -> unit

end
			       
