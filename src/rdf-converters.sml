
structure TurtleNTriplesConverter =
    RdfConverterStreamFn(struct
                          structure Parser = TurtleStreamParser
                          structure Serialiser = NTriplesSerialiser
                          end)

(* !!! + converters between file formats identified by extension *)

                        
