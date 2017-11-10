
structure TurtleExporter :> STORE_EXPORTER where type store = Store.t = struct

    type store = Store.t

    structure TurtleSerialiser = TurtleSerialiserFn(struct
                                                     structure P = PrefixTable
                                                     structure M = Store
                                                     end)
                     
    fun save_to_stream store stream =
        let val serialiser = TurtleSerialiser.new
                                 (Store.get_prefix_table store, store)
                                 stream
        in
            TurtleSerialiser.finish
                (TurtleSerialiser.serialise (serialiser, Store.enumerate store))
        end

    fun save_to_file store filename =
        let val stream = TextIO.openOut filename
            val _ = save_to_stream store stream
        in
            TextIO.closeOut stream
        end
            
end

                                                   
