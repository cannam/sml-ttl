
structure RdfStandardIRIs = struct

    val prefixXsdStr = "http://www.w3.org/2001/XMLSchema#"
    val prefixRdfStr = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

    val prefixXsd = Iri.fromString (prefixXsdStr)
    val prefixRdf = Iri.fromString (prefixRdfStr)

    val iriTypeString = Iri.fromString (prefixXsdStr ^ "string")
    val iriTypeBoolean = Iri.fromString (prefixXsdStr ^ "boolean")
    val iriTypeDouble  = Iri.fromString (prefixXsdStr ^ "double")
    val iriTypeDecimal = Iri.fromString (prefixXsdStr ^ "decimal")
    val iriTypeInteger = Iri.fromString (prefixXsdStr ^ "integer")

    val iriRdfType = Iri.fromString (prefixRdfStr ^ "type")
    val iriRdfFirst = Iri.fromString (prefixRdfStr ^ "first")
    val iriRdfRest = Iri.fromString (prefixRdfStr ^ "rest")
    val iriRdfNil = Iri.fromString (prefixRdfStr ^ "nil")

end
			 
