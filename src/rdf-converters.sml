
structure TurtleNTriplesConverter =
    RdfConverterStreamFn(struct
                          structure Parser = TurtleStreamParser
                          structure Serialiser = NTriplesSerialiser
                          end)

