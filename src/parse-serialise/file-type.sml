
structure FileType = struct

    datatype t = TURTLE | NTRIPLES | OTHER

    fun type_of filename =
             case FileExtension.extension_of filename of
                 "ttl" => TURTLE
               | "n3" => NTRIPLES
               | "ntriples" => NTRIPLES
               | "nt" => NTRIPLES
               | other => OTHER

end
                              
