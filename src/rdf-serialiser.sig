
signature RDF_STREAM_SERIALISER = sig

    type prefix = RdfTriple.prefix
    type triple = RdfTriple.triple

    datatype entry =
	     PREFIX of prefix |
	     TRIPLE of triple
	     
    type t

(*!!! + base iri *)
             
    val create_for_stream : TextIO.outstream -> t
    val serialise : t * entry -> t

end

signature RDF_SERIALISER = sig

    type prefix = RdfTriple.prefix
    type triple = RdfTriple.triple

(*!!! + base iri *)

    val serialise_to_stream : TextIO.outstream -> prefix list * triple list -> unit
    val serialise_to_file : string -> prefix list * triple list -> unit

end
			       
