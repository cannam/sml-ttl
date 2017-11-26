		    
structure RdfTriple :> RDF_TRIPLE where type iri = Iri.t = struct

    open RdfNode
			    
    type triple = node * node * node

    type iri = Iri.t

    fun string_of_triple (a,b,c) =
	"(" ^ (string_of_node a) ^
	"," ^ (string_of_node b) ^
	"," ^ (string_of_node c) ^
	")"

    (* again this is for debug, not for serialisation *)
    fun string_of_triples tt =
        String.concatWith "\n" (map string_of_triple tt)

    fun compare ((s1, p1, o1), (s2, p2, o2)) =
        case RdfNode.compare (s1, s2) of
            GREATER => GREATER
          | LESS => LESS
          | EQUAL =>
            case RdfNode.compare (p1, p2) of
                GREATER => GREATER
              | LESS => LESS
              | EQUAL =>
                RdfNode.compare (o1, o2)
                                
end

