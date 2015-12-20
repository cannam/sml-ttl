
signature TESTS = sig

    type test = string * (unit -> bool)
    type test_suite = string * test list
                   
    val tests : test_suite

end
                      
