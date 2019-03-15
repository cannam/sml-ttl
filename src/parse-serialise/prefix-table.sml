                               
structure IriTrie :> TRIE_MAP where type key = Iri.t = struct

    structure WordVectorTrieMap
        = VectorMTrieMapFn(struct
                            type t = word
                            val compare = Word.compare
                            end)

    type 'a trie = 'a WordVectorTrieMap.trie
    type key = Iri.t

    (*!!! should be wrapper fn in trie for this *)

    val empty = WordVectorTrieMap.empty
    val isEmpty = WordVectorTrieMap.isEmpty

    fun explode iri = WdString.toVector (Iri.toWideString iri)
    fun implode ws = Iri.fromWideString (WdString.fromVector ws)
                      
    fun insert (trie, s, v) =
        WordVectorTrieMap.insert (trie, explode s, v)

    fun update (trie, s, f) =
        WordVectorTrieMap.update (trie, explode s, f)
                              
    fun remove (trie, s) =
        WordVectorTrieMap.remove (trie, explode s)

    fun contains (trie, s) =
        WordVectorTrieMap.contains (trie, explode s)

    fun find (trie, s) =
        WordVectorTrieMap.find (trie, explode s)
                 
    fun lookup (trie, s) =
        WordVectorTrieMap.lookup (trie, explode s)

    val foldl = WordVectorTrieMap.foldl
           
    fun foldli f acc trie =
        WordVectorTrieMap.foldli (fn (e, v, acc) => f (implode e, v, acc))
                                 acc trie

    fun enumerate trie =
        List.map (fn (k, v) => (implode k, v))
                 (WordVectorTrieMap.enumerate trie)

    fun foldlPrefixMatch f acc (trie, s) =
        WordVectorTrieMap.foldlPrefixMatch f acc (trie, explode s)
                 
    fun foldliPrefixMatch f acc (trie, s) =
        WordVectorTrieMap.foldliPrefixMatch
            (fn (e, v, acc) => f (implode e, v, acc))
            acc (trie, explode s)
                 
    fun prefixMatch (trie, s) =
        List.map (fn (k, v) => (implode k, v))
                 (WordVectorTrieMap.prefixMatch (trie, explode s))

    fun prefixOf (trie, s) =
        implode (WordVectorTrieMap.prefixOf (trie, explode s))

end

structure PrefixTable :> PREFIX_TABLE = struct

    type iri = Iri.t
    type prefix = Prefix.prefix
    type abbreviation = Prefix.abbreviation
    type curie = Prefix.curie

    structure AbbrMap = RedBlackMapFn (struct
                                        type ord_key = abbreviation
                                        val compare = String.compare
                                        end)

    type t = iri AbbrMap.map * abbreviation IriTrie.trie
                        
    val empty : t = (AbbrMap.empty, IriTrie.empty)

    fun removeWith (remover, map, key) =
	case remover (map, key) of (map', _) => map' handle NotFound => map

    fun remove (table as (forward, reverse) : t, abbr) =
	case AbbrMap.find (forward, abbr) of
	    NONE => table
	  | SOME expansion =>
	    (removeWith (AbbrMap.remove, forward, abbr),
	     IriTrie.remove (reverse, expansion))
                    
    fun contains (table : t, abbr) =
	isSome (AbbrMap.find (#1 table, abbr))

    fun add (table as (forward, reverse) : t, (abbr, expansion)) =
        if contains (table, abbr)
        then
            (* replace the existing expansion - the default branch
               doesn't do this properly because the map and trie have
               different behaviour for duplicate inserts, so we handle
               it explicitly *)
            add (remove (table, abbr), (abbr, expansion))
        else
	    (AbbrMap.insert (forward, abbr, expansion),
	     IriTrie.insert (reverse, expansion, abbr))

    fun fromPrefixes prefixes =
        List.foldl (fn ((abbr, e), tab) => add (tab, (abbr, e))) empty prefixes
		    
    fun enumerate (table : t) = AbbrMap.listItemsi (#1 table)
                                                     
    fun expand ((forward, _) : t, curie) =
	case String.fields (fn x => x = #":") curie of
	    [] => Iri.fromString curie
	  | abbr::rest =>
	    case AbbrMap.find (forward, abbr) of
		NONE => Iri.fromString curie
	      | SOME expansion =>
                Iri.addSuffix (expansion,
                               WdString.fromUtf8 (String.concatWith ":" rest))

    fun abbreviate ((_, reverse) : t, iri) =
      let val prefix = IriTrie.prefixOf (reverse, iri)
	  open WdString
	  fun dropPrefix (iri, n) =
  	      implode (List.drop (explode (Iri.toWideString iri), n))
      in
	  if Iri.isEmpty prefix
	  then NONE
	  else
	      case IriTrie.find (reverse, prefix) of
		  NONE => raise Fail ("internal error: prefix found in trie " ^
                                      "but not in reverse map")
		| SOME abbr =>
	          SOME (abbr, toUtf8 (dropPrefix (iri, Iri.size prefix)))
      end
		   
end
