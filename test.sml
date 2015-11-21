
fun check () =
    let open Codepoints
        val str1 = CodepointSet.to_string pname_char
	val cp2  = CodepointSet.from_string str1
        val str2 = CodepointSet.to_string cp2
    in
	if str1 <> str2 then
	    let val diff = CodepointSet.difference (pname_char, cp2)
	    in
		print "CodepointSets differ: difference is:\n";
		print (CodepointSet.to_text diff);
		print "\n";
		raise Fail "CodepointSets differ!"
	    end
	else ()
    end

fun main () =
    (print "pname_local_escapable = \n";
     print (CodepointSet.to_string Codepoints.pname_local_escapable);
     print "\n";
     print "iri_escaped = \n";
     print (CodepointSet.to_string Codepoints.iri_escaped);
     print "\n";
     check ());


