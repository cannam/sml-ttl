		    
structure RdfTriple :> RDF_TRIPLE where type iri = Iri.t = struct

    open RdfNode
			    
    type triple = node * node * node

    type iri = Iri.t

    fun stringOfTriple (a,b,c) =
	"(" ^ (stringOfNode a) ^
	"," ^ (stringOfNode b) ^
	"," ^ (stringOfNode c) ^
	")"

    (* again this is for debug, not for serialisation *)
    fun stringOfTriples tt =
        String.concatWith "\n" (map stringOfTriple tt)

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

