
signature RDF_NODE = sig

    datatype node =
             IRI of string |
             BLANK of int |
             LITERAL of {
		 value : string,
		 dtype : string,
		 lang  : string
             }

    val new_blank_node : unit -> node

    val compare : node * node -> order
				     
end
			 
structure RdfNode :> RDF_NODE = struct

    datatype node =
             IRI of string |
             BLANK of int |
             LITERAL of {
		 value : string,
		 dtype : string,
		 lang  : string
             }

    val bnode_counter = ref 0
		
    fun new_blank_node () =
	let val id = !bnode_counter in
	    bnode_counter := id + 1;
	    BLANK id
	end

    fun compare_backwards (s1, s2) =
        (* we don't need lexicographic ordering for IRI strings;
           because they often have common prefixes, comparing from
           their ends is often faster. !!! When printing and serialising,
           though, it is a bit odd for IRIs to be sorted in order from
           shortest to longest - it may be better to use IRI interning
           for the cases where it really matters (i.e. store indexes)
           and a normal string comparison here *)
        let val n1 = String.size s1
            val n2 = String.size s2
            fun compare' ~1 = EQUAL
              | compare' n = 
                case Char.compare (String.sub (s1, n), String.sub (s2, n)) of
                    LESS => LESS
                  | GREATER => GREATER
                  | EQUAL => compare' (n - 1)
        in
            if n1 < n2 then LESS
            else if n1 > n2 then GREATER
            else compare' (n1 - 1)
        end
            
    fun compare (IRI i1, IRI i2) = compare_backwards (i1, i2) 
      | compare (BLANK b1, BLANK b2) = Int.compare (b1, b2)
      | compare (LITERAL l1, LITERAL l2) =
        (case String.compare (#value l1, #value l2) of
             GREATER => GREATER
           | LESS => LESS
           | EQUAL =>
	     if #dtype l1 <> #dtype l2 then String.compare (#dtype l1, #dtype l2)
	     else if #lang l1 <> #lang l2 then String.compare (#lang l1, #lang l2)
	     else EQUAL)
      | compare (IRI _, _) = LESS
      | compare (BLANK _, IRI _) = GREATER
      | compare (BLANK _, _) = LESS
      | compare (LITERAL _, _) = GREATER
    
end
		    
structure RdfTriple = struct

    open RdfNode
			    
    type triple = node * node * node

    type prefix = string * string

    (* This is debug streaming, not guaranteed to match a standard format *)
    fun string_of_node (IRI iri) = "<" ^ iri ^ ">"
      | string_of_node (BLANK n) = "_:blank" ^ (Int.toString n)
      | string_of_node (LITERAL lit) = "\"" ^ (#value lit) ^ "\"" ^
                                       (if #dtype lit = "" then ""
					else "^^" ^ (#dtype lit)) ^
                                       (if #lang lit = "" then ""
					else "@" ^ (#lang lit))

    fun string_of_triple (a,b,c) =
	"(" ^ (string_of_node a) ^
	"," ^ (string_of_node b) ^
	"," ^ (string_of_node c) ^
	")"

end

