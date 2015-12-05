
structure RdfStandardIRIs = struct

    val prefix_xsd = "http://www.w3.org/2001/XMLSchema#"
    val prefix_rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

    val iri_type_boolean = Iri.fromString (prefix_xsd ^ "boolean")
    val iri_type_double  = Iri.fromString (prefix_xsd ^ "double")
    val iri_type_decimal = Iri.fromString (prefix_xsd ^ "decimal")
    val iri_type_integer = Iri.fromString (prefix_xsd ^ "integer")

    val iri_rdf_type = Iri.fromString (prefix_rdf ^ "type")
    val iri_rdf_first = Iri.fromString (prefix_rdf ^ "first")
    val iri_rdf_rest = Iri.fromString (prefix_rdf ^ "rest")
    val iri_rdf_nil = Iri.fromString (prefix_rdf ^ "nil")

end
			 
