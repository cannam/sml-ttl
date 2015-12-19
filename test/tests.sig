
structure TestTypes = struct

    type test = { test_name : string, test : unit -> bool }
    type test_suite = { suite_name : string, tests : test list }

end

signature TESTS = sig

    type test = { test_name : string, test : unit -> bool }
    type test_suite = { suite_name : string, tests : test list }
                   
    val tests : test_suite

end
                      
