
signature TEST_INDEX_ARG = sig

    structure IX : INDEX
    structure P : INDEX_PICKER
    
end

functor TestIndexFn (Arg : TEST_INDEX_ARG) :> TESTS = struct

    open TestSupport

    structure IX = Arg.IX
    structure P = Arg.P

    open RdfNode

    val name = "index"

    fun test_remind_myself_to_implement_something () = false
                   
    fun tests () = [
        ("remind-myself-to-implement-something",
         test_remind_myself_to_implement_something)
    ]
             
end

structure TestIndex = TestIndexFn(struct
                                   structure IX = Index
                                   structure P = IndexPicker
                                   end)
                                 
