
fun run_test_suite (suite_name, tests) =
    case
        List.mapPartial
            (fn (test_name, test) =>
                if test () then NONE
                else (print ("Test \"" ^ test_name ^ "\" failed\n");
                      SOME test_name))
            tests
     of failed =>
        let val n = length tests
            val m = length failed
        in
            print (suite_name ^ ": " ^
                   (Int.toString (n - m)) ^ "/" ^ (Int.toString n) ^
                   " tests passed\n");
            if m > 0
            then print (suite_name ^
                        ": Failed tests [" ^ (Int.toString m) ^ "]: " ^
                        (String.concatWith " " failed) ^ "\n")
            else ()
        end

val all_tests = [
    TestPrefix.tests
]

fun main () =
    app run_test_suite all_tests

        
