
signature RDF_CONVERTER = sig

    (* Signature for converters that already know, or assume, what
       formats to convert from and to *)
    
    type base_iri = string

    datatype result = CONVERSION_ERROR of string |
                      CONVERTED
                        
    val convert : base_iri -> TextIO.instream -> TextIO.outstream -> result

end

signature RDF_FILE_CONVERTER = sig

    (* Signature for converters that determine the format to be loaded
       based on file metadata, e.g. suffix. These therefore can only
       load from files, not arbitrary text streams. *)

    type base_iri = string

    datatype result = CONVERSION_ERROR of string |
                      CONVERTED
                        
    val convert : base_iri -> string (* infile *) -> string (* outfile *) -> result

end
                              
