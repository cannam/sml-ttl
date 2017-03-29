                             
structure Store :> STORE = struct

    datatype node = datatype RdfNode.node

    type patnode = node option
    type triple = node * node * node
    type pattern = patnode * patnode * patnode
    type iri = Iri.t
					   
    type t = {
	prefixes : PrefixTable.t,
	indexes : Index.t list
    }

    val empty = {
	prefixes = let open PrefixTable RdfStandardIRIs
                   in
	               add (add (empty, "rdf", prefix_rdf), "xsd", prefix_xsd)
                   end,
	indexes = [ Index.new Index.SPO,
		    Index.new Index.POS,
		    Index.new Index.OPS ]
    }

    fun string_of_patnode NONE = "*"
      | string_of_patnode (SOME n) = RdfNode.string_of_node n
                    
    fun string_of_pattern (a,b,c) =
	"(" ^ (string_of_patnode a) ^
	"," ^ (string_of_patnode b) ^
	"," ^ (string_of_patnode c) ^
	")"
                    
    fun any_index ({ indexes, ... } : t) = hd indexes (* when any index is ok *)

    fun contains (store, triple) =
	Index.contains (any_index store, triple)

    fun map_indexes f ({ prefixes, indexes } : t) =
	{ prefixes = prefixes, indexes = map f indexes }
	  
    fun add (store, triple) =
	map_indexes (fn ix => Index.add (ix, triple)) store 

    fun remove (store, triple) =
	map_indexes (fn ix => Index.remove (ix, triple)) store

    fun foldl_match f acc ({ prefixes, indexes } : t, pattern) =
        let val index = IndexPicker.pick_index (indexes, pattern)
        in
            Log.info (fn () => ["Store: pattern %, index \"%\"",
                                Log.S (string_of_pattern pattern),
                                Log.S (Index.name index)]);
            Index.foldl_match f acc (index, pattern)
        end

    fun foldl f acc store =
        Index.foldl_match f acc (any_index store, (NONE, NONE, NONE))
                         
    fun match pattern =
        let val result = foldl_match (op::) [] pattern
        in
            Log.info (fn () => ["Store: matched % results",
                                Log.I (length result)]);
            result
        end

    val enumerate = foldl (op::) []

    fun add_prefix ({ prefixes, indexes } : t, prefix, expansion) =
        { prefixes = PrefixTable.add (prefixes, prefix, expansion),
          indexes = indexes }
	    
    fun remove_prefix (store as { prefixes, indexes } : t, prefix) =
        { prefixes = PrefixTable.remove (prefixes, prefix),
          indexes = indexes }

    fun contains_prefix ({ prefixes, ... } : t, prefix) =
        PrefixTable.contains (prefixes, prefix)
		    
    fun enumerate_prefixes ({ prefixes, ... } : t) =
        PrefixTable.enumerate prefixes

    fun expand ({ prefixes, ... } : t, curie) =
        PrefixTable.expand (prefixes, curie)

    fun abbreviate ({ prefixes, ... } : t, iri) =
        PrefixTable.abbreviate (prefixes, iri)

end
