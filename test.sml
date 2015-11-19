
fun main () =
    (print "pname_char_codepoints = \n";
     print (CodepointSet.to_string pname_char_codepoints);
     print "\n";
     print "pname_local_escapable_codepoints = \n";
     print (CodepointSet.to_string pname_local_escapable_codepoints);
     print "\n";
     print "iri_escaped_codepoints = \n";
     print (CodepointSet.to_string iri_escaped_codepoints);
     print "\n");


