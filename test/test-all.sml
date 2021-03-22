
fun reportException name msg =
    (print ("*** Caught exception in test \"" ^ name ^ "\": " ^ msg ^ "\n");
     false)
          
fun runTestSuite (suiteName, tests) =
    case
        List.mapPartial
            (fn (testName, test) =>
                if (test ()
                    handle Fail msg => reportException testName msg
                         | IO.Io { name, ... } =>
                           reportException testName ("IO failure: " ^ name)
                         | ex => reportException testName (exnMessage ex))
                then NONE
                else (print ("*** Test \"" ^ testName ^ "\" failed\n");
                      SOME testName))
            tests
     of failed =>
        let val n = length tests
            val m = length failed
        in
            print (suiteName ^ ": " ^
                   (Int.toString (n - m)) ^ "/" ^ (Int.toString n) ^
                   " tests passed\n");
            if m > 0
            then (print (suiteName ^
                         ": Failed tests [" ^ (Int.toString m) ^ "]: " ^
                         (String.concatWith " " failed) ^ "\n");
                  OS.Process.failure)
            else OS.Process.success
        end
            
fun ttl_tests () = [
    (TestPrefix.name, TestPrefix.tests ()),
    (TestProperty.name, TestProperty.tests ()),
    (TestIndex.name, TestIndex.tests ()),
    (TestStore.name, TestStore.tests ()),
    (TestCollection.name, TestCollection.tests ()),
    (TestTurtleParser.name, TestTurtleParser.tests ()),
    (TestTurtleSpec.name, TestTurtleSpec.tests ()),
    (TestCurated.name, TestCurated.tests ())
]
	 
fun usage () =
    let open TextIO in
	output (stdErr,
	    "Usage:\n" ^
            "    test-all [-v]\n");
        raise Fail "Incorrect arguments specified"
    end

fun handleArgs args =
    case args of
        "-v"::rest => (Log.setLogLevel Log.INFO ; handleArgs rest)
      | [] => foldl (fn (test, acc) =>
                        let val code = runTestSuite test
                        in
                            if OS.Process.isSuccess code
                            then acc
                            else code
                        end)
                    OS.Process.success
                    (ttl_tests ())
      | _ => usage ()
           
fun main () =
    OS.Process.exit (handleArgs (CommandLine.arguments ()))
    handle Fail msg =>
           (TextIO.output (TextIO.stdErr, "Exception: " ^ msg ^ "\n");
            OS.Process.exit OS.Process.failure)


        
