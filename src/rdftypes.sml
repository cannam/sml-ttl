
structure RdfTypes = struct

    val prefix_xsd = "http://www.w3.org/2001/XMLSchema#"
    val prefix_rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

    val iri_type_boolean = prefix_xsd ^ "boolean"
    val iri_type_double  = prefix_xsd ^ "double"
    val iri_type_decimal = prefix_xsd ^ "decimal"
    val iri_type_integer = prefix_xsd ^ "integer"

    val iri_rdf_type = prefix_rdf ^ "type"
    val iri_rdf_first = prefix_rdf ^ "first"
    val iri_rdf_rest = prefix_rdf ^ "rest"
    val iri_rdf_nil = prefix_rdf ^ "nil"

end
			 
