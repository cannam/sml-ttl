
fun check () =
    let val str1 = CodepointSet.to_string pname_char_codepoints ^ "x"
	val cp2  = CodepointSet.from_string str1
    in
	if not (CodepointSet.equal (pname_char_codepoints, cp2)) then
	    let val diff = CodepointSet.difference (pname_char_codepoints, cp2)
	    in
		print "CodepointSets differ: difference is:\n";
		print (CodepointSet.to_string diff);
		print "\n";
		raise Fail "CodepointSets differ!"
	    end
	else ()
    end

fun main () =
    (print "pname_char_codepoints = \n";
     print (CodepointSet.to_string pname_char_codepoints);
     print "\n";
     print "pname_local_escapable_codepoints = \n";
     print (CodepointSet.to_string pname_local_escapable_codepoints);
     print "\n";
     print "iri_escaped_codepoints = \n";
     print (CodepointSet.to_string iri_escaped_codepoints);
     print "\n";
     check ());


