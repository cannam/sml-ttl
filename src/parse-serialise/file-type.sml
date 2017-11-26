
structure FileType = struct

    datatype format = TURTLE | NTRIPLES | RDFXML | OTHER

    fun formatForExtension e =
        case e of
            "ttl" => TURTLE
          | "n3" => TURTLE
          | "ntriples" => NTRIPLES
          | "nt" => NTRIPLES
          | "rdf" => RDFXML
          | other => OTHER
                              
    fun extensionsForFormat t =
        case t of
            TURTLE => ["ttl","n3"]
          | NTRIPLES => ["ntriples", "nt"]
          | RDFXML => ["rdf"]
          | OTHER => []

    fun nameForFormat t =
        case t of
            TURTLE => "Turtle"
          | NTRIPLES => "NTriples"
          | RDFXML => "RDF/XML"
          | OTHER => ""

    fun nameForExtension e =
        nameForFormat (formatForExtension e)
        
    fun formatOf filename =
        formatForExtension (FileExtension.extensionOf filename)

end
                              
