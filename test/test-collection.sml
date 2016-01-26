
signature TEST_COLLECTION_ARG = sig

    structure R : RDF_COLLECTION
    structure C : STORE_COLLECTION
    
end

functor TestCollectionFn (Arg : TEST_COLLECTION_ARG) :> TESTS = struct

    open TestSupport

    structure R = Arg.R
    structure C = Arg.C
             
    val name = "collection"

    fun test_make_empty () =
        check RdfTriple.string_of_triples
              (R.collection_of_nodes [], [])
                   
    fun tests () = [
        ("make-empty", test_make_empty)
    ]

end
                                                              
structure TestCollection = TestCollectionFn(struct
                                             structure R = RdfCollection
                                             structure C = StoreCollection
                                             end)

