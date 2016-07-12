
fun report_exception name msg =
    (print ("*** Caught exception in test \"" ^ name ^ "\": " ^ msg ^ "\n");
     false)
          
fun run_test_suite (suite_name, tests) =
    case
        List.mapPartial
            (fn (test_name, test) =>
                if (test ()
                    handle Fail msg => report_exception test_name msg
                         | IO.Io { name, ... } =>
                           (*!!! can we get more info from Exception? *)
                           report_exception test_name ("IO failure: " ^ name)
                         | ex => report_exception test_name "Exception caught")
                then NONE
                else (print ("*** Test \"" ^ test_name ^ "\" failed\n");
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
    (TestPrefix.name, TestPrefix.tests ()),
    (TestProperty.name, TestProperty.tests ()),
    (TestIndex.name, TestIndex.tests ()),
    (TestStore.name, TestStore.tests ()),
    (TestCollection.name, TestCollection.tests ()),
    (TestTurtleParser.name, TestTurtleParser.tests ()),
    (TestTurtleSpec.name, TestTurtleSpec.tests ())
]
	 
fun usage () =
    let open TextIO in
	output (stdErr,
	    "Usage:\n" ^
            "    test-all [-v]\n");
        raise Fail "Incorrect arguments specified"
    end

fun handle_args args =
    case args of
        "-v"::rest => (Log.set_log_level Log.INFO ; handle_args rest)
      | [] => app run_test_suite all_tests
      | _ => usage ()
           
fun main () =
    handle_args (CommandLine.arguments ())
    handle Fail msg => TextIO.output (TextIO.stdErr, "Exception: " ^ msg ^ "\n")

        
