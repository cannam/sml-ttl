
signature RDF_CONVERTER = sig

    type prefix = RdfTriple.prefix
    type triple = RdfTriple.triple

    type base_iri = string

    datatype result = CONVERSION_ERROR of string |
                      CONVERTED
                        
    val convert : base_iri -> TextIO.instream -> TextIO.outstream -> result

end
                              
