
signature PREFIX = sig

    type iri

    (** The abbreviation that expands to an IRI prefix, e.g. "xsd" *)
    type abbreviation = string         (* e.g. "xsd" *)

    (** The abbreviation with its expansion, e.g. ("xsd",
        Iri.fromString "http://www.w3.org/2001/XMLSchema#") *)
    type prefix = abbreviation * iri

    (** A shortened IRI in UTF-8 text form, e.g. "xsd:boolean" *)
    type curie = string

    (** Convert a prefix to a string for debug output purposes.
        Not guaranteed to match any standard format *)
    val stringOfPrefix : prefix -> string

end

