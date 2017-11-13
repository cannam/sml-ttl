
signature RDF_CONVERTER_BASE = sig
    
    type base_iri = string

end

(** An RDF_CONVERTER converts from one file or stream directly to
    another. It may use a store or any other magic in between, but the
    API does not make the intermediate triples visible.

    The RDF_CONVERTER signature is for converters that already know,
    or assume, what formats to convert from and to. (Unlike
    RDF_FILE_CONVERTER, which determines these based on file
    extensions.)
*)
signature RDF_CONVERTER = sig

    include RDF_CONVERTER_BASE

    datatype result =
             (** Error produced during parsing or serialisation *)
             CONVERSION_ERROR of string |
             (** Successful conversion *) (*!!! should be OK like the store io stuff? *)
             CONVERTED
    
    val convert : base_iri * TextIO.instream ->
                  base_iri * TextIO.outstream ->
                  result

end
    
(** Signature for converters that determine the formats to be loaded
    and saved based on file metadata, e.g. suffix. This therefore can
    only read and write from and to files, not arbitrary text streams.
*)
signature RDF_FILE_CONVERTER = sig

    include RDF_CONVERTER_BASE

    datatype result =
             (** Immediate failure due to unknown or unsupported input format *)
             INPUT_FORMAT_NOT_SUPPORTED |
             (** Immediate failure due to unknown or unsupported output format *)
             OUTPUT_FORMAT_NOT_SUPPORTED |
             (** Error caught from system code, e.g. file not found *)
             SYSTEM_ERROR of string |
             (** Error produced during parsing or serialisation *)
             CONVERSION_ERROR of string |
             (** Successful conversion *)
             CONVERTED

    val convert : base_iri * string -> (* input iri + filename *)
                  base_iri * string -> (* output iri + filename *)
                  result

end
                              
