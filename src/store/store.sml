                             
structure Store :> STORE = struct

    datatype node = datatype RdfNode.node

    open RdfLog
                                 
    type patnode = node option
    type triple = node * node * node
    type pattern = patnode * patnode * patnode

    type iri = Iri.t
    type prefix = Prefix.prefix
    type abbreviation = Prefix.abbreviation
    type curie = string
					   
    type t = {
	prefixes : PrefixTable.t,
	indexes : Index.t list
    }

    val empty = {
	prefixes = PrefixTable.empty,
	indexes = [ Index.new Index.SPO,
		    Index.new Index.POS,
		    Index.new Index.OPS ]
    }

    fun stringOfPatnode NONE = "*"
      | stringOfPatnode (SOME n) = RdfNode.stringOfNode n
                    
    fun stringOfPattern (a,b,c) =
	"(" ^ (stringOfPatnode a) ^
	"," ^ (stringOfPatnode b) ^
	"," ^ (stringOfPatnode c) ^
	")"
                    
    fun anyIndex ({ indexes, ... } : t) = hd indexes (* when any index is ok *)

    fun contains (store, triple) =
	Index.contains (anyIndex store, triple)

    fun mapIndexes f ({ prefixes, indexes } : t) =
	{ prefixes = prefixes, indexes = map f indexes }
	  
    fun add (store, triple) =
	mapIndexes (fn ix => Index.add (ix, triple)) store 

    fun remove (store, triple) =
	mapIndexes (fn ix => Index.remove (ix, triple)) store

    fun foldlMatch f acc ({ prefixes, indexes } : t, pattern) =
        let val index = IndexPicker.pickIndex (indexes, pattern)
        in
            debug (fn () => ["Store: pattern %1, index \"%2\"",
                             S (stringOfPattern pattern),
                             S (Index.name index)]);
            Index.foldlMatch f acc (index, pattern)
        end

    fun foldl f acc store =
        Index.foldlMatch f acc (anyIndex store, (NONE, NONE, NONE))
                         
    fun match pattern =
        let val result = foldlMatch (op::) [] pattern
        in
            debug (fn () => ["Store: matched %1 results",
                             I (length result)]);
            result
        end

    val enumerate = foldl (op::) []

    fun addPrefix ({ prefixes, indexes } : t, prefix) =
        { prefixes = PrefixTable.add (prefixes, prefix),
          indexes = indexes }
	    
    fun removePrefix (store as { prefixes, indexes } : t, abbr) =
        { prefixes = PrefixTable.remove (prefixes, abbr),
          indexes = indexes }

    fun containsPrefix ({ prefixes, ... } : t, abbr) =
        PrefixTable.contains (prefixes, abbr)
		    
    fun enumeratePrefixes ({ prefixes, ... } : t) =
        PrefixTable.enumerate prefixes

    fun expand ({ prefixes, ... } : t, curie) =
        PrefixTable.expand (prefixes, curie)

    fun abbreviate ({ prefixes, ... } : t, iri) =
        PrefixTable.abbreviate (prefixes, iri)

    fun getPrefixTable ({ prefixes, ... } : t) =
        prefixes
                               
end
