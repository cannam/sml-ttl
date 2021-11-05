
structure TestDir = struct

    fun testFileDir name =
        let val dir = "test/" ^ name
            val candidateDirectories = ["sml-ttl", "."]
            val checkFile = ".found"
	in
	    case foldl (fn (candidate, SOME acceptable) => SOME acceptable
                       | (candidate, NONE) =>
                         if OS.FileSys.access (candidate ^ "/" ^ dir ^ "/" ^ checkFile,
                                               [OS.FileSys.A_READ])
			 then SOME candidate
			 else NONE)
		       NONE
		       candidateDirectories of
		NONE => raise Fail ("Test file directory " ^ dir ^ " not found: make sure you are running the tests from the correct directory. (Looked within: " ^ String.concatWith ", " (map (fn d => "'" ^ d ^ "'") candidateDirectories) ^ ")")
	      | SOME acceptable => acceptable ^ "/" ^ dir
        end

end
