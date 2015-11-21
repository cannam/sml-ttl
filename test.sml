
open Codepoints

fun check () =
    let val str1 = CodepointSet.to_string pname_char_codepoints
	val cp2  = CodepointSet.from_string str1
        val str2 = CodepointSet.to_string cp2
    in
	if str1 <> str2 then
	    let val diff = CodepointSet.difference (pname_char_codepoints, cp2)
	    in
		print "CodepointSets differ: difference is:\n";
		print (CodepointSet.to_text diff);
		print "\n";
		raise Fail "CodepointSets differ!"
	    end
	else ()
    end

fun main () =
    (print "pname_local_escapable_codepoints = \n";
     print (CodepointSet.to_string pname_local_escapable_codepoints);
     print "\n";
     print "iri_escaped_codepoints = \n";
     print (CodepointSet.to_string iri_escaped_codepoints);
     print "\n";
     check ());


