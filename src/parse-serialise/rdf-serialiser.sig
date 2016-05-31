
signature RDF_SERIALISER = sig

    type prefix = RdfTriple.prefix
    type triple = RdfTriple.triple

(*!!! + base iri *)
                      
    val serialise : TextIO.outstream * prefix list * triple list -> unit

end

signature RDF_STREAM_SERIALISER = sig

    type prefix = RdfTriple.prefix
    type triple = RdfTriple.triple

    datatype entry =
	     PREFIX of prefix |
	     TRIPLE of triple
	     
    type t

(*!!! + base iri *)
             
    val new : TextIO.outstream -> t
    val serialise : t * entry -> t

end
			       
(*!!! + file serialiser (i.e. using extension, analogous to store file exporter) *)
