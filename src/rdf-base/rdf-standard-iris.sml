
structure RdfStandardIRIs = struct

    val prefix_xsd_str = "http://www.w3.org/2001/XMLSchema#"
    val prefix_rdf_str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

    val prefix_xsd = Iri.fromString (prefix_xsd_str)
    val prefix_rdf = Iri.fromString (prefix_rdf_str)

    val iri_type_boolean = Iri.fromString (prefix_xsd_str ^ "boolean")
    val iri_type_double  = Iri.fromString (prefix_xsd_str ^ "double")
    val iri_type_decimal = Iri.fromString (prefix_xsd_str ^ "decimal")
    val iri_type_integer = Iri.fromString (prefix_xsd_str ^ "integer")

    val iri_rdf_type = Iri.fromString (prefix_rdf_str ^ "type")
    val iri_rdf_first = Iri.fromString (prefix_rdf_str ^ "first")
    val iri_rdf_rest = Iri.fromString (prefix_rdf_str ^ "rest")
    val iri_rdf_nil = Iri.fromString (prefix_rdf_str ^ "nil")

end
			 
