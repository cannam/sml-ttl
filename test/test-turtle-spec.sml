
functor TestTurtleSpecFn (L: STORE_FORMAT_LOADER) : TESTS = struct

    open TestSupport RdfTriple

    val test_file_dir = "test/spec"
    val out_file_dir = "test/out"

    fun test_file filename = test_file_dir ^ "/" ^ filename ^ ".ttl"
                     
    val base_iri = "http://example/base/"

    fun tests_from_store store = []
                       
    fun tests_from_manifest name =
        case L.load_file_as_new_store "" (test_file name)
             handle IO.Io { name, ... } =>
                    L.LOAD_ERROR ("failed to open file \"" ^ name ^ "\"")
         of
            L.OK store => tests_from_store store
          | L.LOAD_ERROR err =>
            [("manifest-load-failed",
              (fn () =>
                  (print ("--- Spec test manifest load failed: " ^ err ^ "\n");
                   false)))]
                       
    val tests = (
        "turtle-spec",
        tests_from_manifest "manifest"
    )
                     
end

structure TestTurtleSpec = TestTurtleSpecFn(TurtleLoader)

                                           
