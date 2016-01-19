
signature RDF_NODE = sig

    type iri = Iri.t

    datatype node =
             IRI of iri |
             BLANK of int |
             LITERAL of {
		 value : string,
		 dtype : iri,
		 lang  : string
             }

    val new_blank_node : unit -> node

    (* Compare two nodes rapidly and consistently. The ordering is
       guaranteed to place IRI nodes first, blank nodes next, and
       literal nodes last. Within node type no particular ordering is
       assured, e.g. IRIs may not be lexicographically ordered but
       instead ordered on some internal key. *)
    val compare : node * node -> order
				     
end
			 
structure RdfNode :> RDF_NODE = struct

    type iri = Iri.t

    datatype node =
             IRI of iri |
             BLANK of int |
             LITERAL of {
		 value : string,
		 dtype : iri,
		 lang  : string
             }

    val bnode_counter = ref 0
		
    fun new_blank_node () =
	let val id = !bnode_counter in
	    bnode_counter := id + 1;
	    BLANK id
	end

    fun compare (IRI i1, IRI i2) = Iri.compare (i1, i2) 
      | compare (BLANK b1, BLANK b2) = Int.compare (b1, b2)
      | compare (LITERAL l1, LITERAL l2) =
        (case String.compare (#value l1, #value l2) of
             GREATER => GREATER
           | LESS => LESS
           | EQUAL =>
             case Iri.compare (#dtype l1, #dtype l2) of
                 GREATER => GREATER
               | LESS => LESS
               | EQUAL => String.compare (#lang l1, #lang l2))
      | compare (IRI _, _) = LESS
      | compare (BLANK _, IRI _) = GREATER
      | compare (BLANK _, _) = LESS
      | compare (LITERAL _, _) = GREATER
    
end
		    
structure RdfTriple = struct

    open RdfNode
			    
    type triple = node * node * node

    type prefix = string * string

    fun string_of_prefix (pre, exp) =
        "\"" ^ pre ^ "\" -> \"" ^ exp ^ "\""
                               
    (* This is debug streaming, not guaranteed to match a standard format *)
    (*!!! should be in node? *)
    fun string_of_node (IRI iri) = "<" ^ (Iri.toString iri) ^ ">"
      | string_of_node (BLANK n) = "_:blank" ^ (Int.toString n)
      | string_of_node (LITERAL lit) =
        "\"" ^ (#value lit) ^ "\"" ^
        (if #dtype lit = Iri.empty_iri then ""
	 else "^^" ^ (Iri.toString (#dtype lit))) ^
        (if #lang lit = "" then ""
	 else "@" ^ (#lang lit))

    fun string_of_triple (a,b,c) =
	"(" ^ (string_of_node a) ^
	"," ^ (string_of_node b) ^
	"," ^ (string_of_node c) ^
	")"

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

