
structure FileType = struct

    datatype format = TURTLE | NTRIPLES | RDFXML | OTHER

    fun format_for_extension e =
        case e of
            "ttl" => TURTLE
          | "n3" => TURTLE
          | "ntriples" => NTRIPLES
          | "nt" => NTRIPLES
          | "rdf" => RDFXML
          | other => OTHER
                              
    fun extensions_for_format t =
        case t of
            TURTLE => ["ttl","n3"]
          | NTRIPLES => ["ntriples", "nt"]
          | RDFXML => ["rdf"]
          | OTHER => []

    fun name_for_format t =
        case t of
            TURTLE => "Turtle"
          | NTRIPLES => "NTriples"
          | RDFXML => "RDF/XML"
          | OTHER => ""

    fun name_for_extension e =
        name_for_format (format_for_extension e)
        
    fun format_of filename =
        format_for_extension (FileExtension.extension_of filename)

end
                              
