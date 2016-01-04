                             
structure Store :> STORE = struct

    datatype node = datatype RdfNode.node
    datatype patnode = datatype Index.patnode

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

    fun any_index ({ indexes, ... } : t) = hd indexes (* when any index is ok *)

    fun contains (store, triple) =
	Index.contains (any_index store, triple)

    fun map_indexes f ({ prefixes, indexes } : t) =
	{ prefixes = prefixes, indexes = map f indexes }
	  
    fun add (store, triple) =
	map_indexes (fn ix => Index.add (ix, triple)) store 

    fun remove (store, triple) =
	map_indexes (fn ix => Index.remove (ix, triple)) store

    fun fold_match f acc ({ prefixes, indexes } : t, pattern) =
        Index.fold_match f acc
                         (IndexPicker.pick_index (indexes, pattern), pattern)

    fun foldl f acc store =
        Index.fold_match f acc
                         (any_index store, (WILDCARD, WILDCARD, WILDCARD))
                         
    val match = fold_match (op::) []

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
