
open RdfTriple

fun check () =
    let open TurtleCodepoints
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
    handle Fail x => print ("exception caught: " ^ x ^ "\n")
(*
fun parse () =
    let open TurtleParser
        val instr = "@prefix : <http://blather/blah#> .  \n  <a> a :c .\n :c <something> \"this\".\n <b> :has_friends [ :name 'Chris' ; :id '40213' ]."
    in
        case parse_string "file:///blah" instr of
            PARSE_ERROR e => print ("error: " ^ e ^ "\n")
          | PARSED p =>
            (print "parsed the following triple(s):\n";
             app (fn t => print ((string_of_triple t) ^ "\n")) (#triples p))
    end
*)        
fun main () =
    (print "pname_local_escapable = \n";
     print (CodepointSet.to_string TurtleCodepoints.pname_local_escapable);
     print "\n";
     print "iri_escaped = \n";
     print (CodepointSet.to_string TurtleCodepoints.iri_escaped);
     print "\n";
(*     parse (); *)
     check ());


