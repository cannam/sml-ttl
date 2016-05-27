
signature RDF_CONVERTER_BASE = sig
    
    type base_iri = string

    datatype result = CONVERSION_ERROR of string |
                      CONVERTED

end

signature RDF_CONVERTER = sig

    include RDF_CONVERTER_BASE
    
    (* An RDF_CONVERTER converts from one file or stream directly to
       another. It may use a store or any other magic in between, but
       the API does not make the intermediate triples visible.

       The RDF_CONVERTER signature is for converters that already
       know, or assume, what formats to convert from and to. (Unlike
       RDF_FILE_CONVERTER, which determines these based on file
       extensions.) *)
                        
    val convert : base_iri -> TextIO.instream -> TextIO.outstream -> result

end

signature RDF_FILE_CONVERTER = sig

    include RDF_CONVERTER_BASE
    
    (* Signature for converters that determine the format to be loaded
       based on file metadata, e.g. suffix. These therefore can only
       load from files, not arbitrary text streams. *)

    val convert : base_iri -> string (* infile *) -> string (* outfile *) -> result

end
                              
