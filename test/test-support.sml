
structure TestSupport = struct

    type test = string * (unit -> bool)
    type testSuite = string * test list

    fun report converter (obtained, expected) =
        print ("--- Expected " ^ (converter expected)
               ^ "\n--- Obtained " ^ (converter obtained) ^ "\n")
                                    
    fun check converter (a, b) =
        if a = b then true
        else (report converter (a, b); false)

    fun checkPairs converter pairs =
        case List.filter (op<>) pairs of
            [] => true
          | unequal => (app (report converter) unequal; false)

    fun checkLists converter (a, b) =
        let fun checkLists' ([], []) = true
              | checkLists' (a', b') =
                check converter (hd a', hd b') andalso
                checkLists' (tl a', tl b')
        in
            if (List.length a <> List.length b)
            then 
                (print ("--- Lists have differing lengths (expected " ^
                        (Int.toString (List.length b)) ^
                        ", obtained " ^
                        (Int.toString (List.length a)) ^ ": [" ^
			(String.concatWith "," (List.map converter a)) ^
                        "])\n");
                 false)
            else
                checkLists' (a, b)
        end

    fun checkSets converter greater (a, b) =
        checkLists converter
                    (ListMergeSort.sort greater a,
                     ListMergeSort.sort greater b)
                           
end
