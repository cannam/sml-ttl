
functor TestPrefixFn (P: PREFIX_TABLE) : TESTS = struct

    open TestTypes

    val tests = {
        suite_name = "prefix",
        tests = [
            { test_name = "blah", test = fn () => false }
        ]
    }
             
end
                                                    
structure TestPrefix = TestPrefixFn(PrefixTable)
