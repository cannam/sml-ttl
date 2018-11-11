
structure IriTrie :> TRIE where type entry = Iri.t = struct

    structure WordListTrie = ListMTrieFn(struct
				          type t = word
				          val compare = Word.compare
				          end)

    type t = WordListTrie.t
    type trie = t
    type entry = Iri.t

    val empty = WordListTrie.empty
    val isEmpty = WordListTrie.isEmpty

    fun explode iri = WdString.explode (Iri.toWideString iri)
    fun implode ws = Iri.fromWideString (WdString.implode ws)
                    
    fun add (trie, s) =
        WordListTrie.add (trie, explode s)

    fun contains (trie, s) =
        WordListTrie.contains (trie, explode s)
                         
    fun remove (trie, s) =
        WordListTrie.remove (trie, explode s)

    fun foldl f acc trie =
        WordListTrie.foldl (fn (e, acc) => f (implode e, acc)) acc trie

    fun enumerate trie =
        List.map implode (WordListTrie.enumerate trie)

    fun foldlPrefixMatch f acc (trie, s) =
        WordListTrie.foldlPrefixMatch (fn (e, acc) => f (implode e, acc))
                                        acc (trie, explode s)
                 
    fun prefixMatch (trie, s) =
        List.map implode (WordListTrie.prefixMatch (trie, explode s))

    fun prefixOf (trie, s) =
        implode (WordListTrie.prefixOf (trie, explode s))

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

    structure IriMap = RedBlackMapFn (struct
                                       type ord_key = iri
                                       val compare = Iri.compare
                                       end)

    type t = iri AbbrMap.map * abbreviation IriMap.map * IriTrie.t
                        
    val empty : t = (AbbrMap.empty, IriMap.empty, IriTrie.empty)

    fun removeWith (remover, map, key) =
	case remover (map, key) of (map', _) => map' handle NotFound => map

    fun remove (table as (forward, reverse, trie) : t, abbr) =
	case AbbrMap.find (forward, abbr) of
	    NONE => table
	  | SOME expansion =>
	    (removeWith (AbbrMap.remove, forward, abbr),
             removeWith (IriMap.remove, reverse, expansion),
	     IriTrie.remove (trie, expansion))
                    
    fun contains (table : t, abbr) =
	isSome (AbbrMap.find (#1 table, abbr))

    fun add (table as (forward, reverse, trie) : t, (abbr, expansion)) =
        if contains (table, abbr)
        then
            (* replace the existing expansion - the default branch
               doesn't do this properly because the map and trie have
               different behaviour for duplicate inserts, so we handle
               it explicitly *)
            add (remove (table, abbr), (abbr, expansion))
        else
	    (AbbrMap.insert (forward, abbr, expansion),
	     IriMap.insert (reverse, expansion, abbr),
	     IriTrie.add (trie, expansion))

    fun fromPrefixes prefixes =
        List.foldl (fn ((abbr, e), tab) => add (tab, (abbr, e))) empty prefixes
		    
    fun enumerate (table : t) = AbbrMap.listItemsi (#1 table)
                                                     
    fun expand ((forward, _, _) : t, curie) =
	case String.fields (fn x => x = #":") curie of
	    [] => Iri.fromString curie
	  | abbr::rest =>
	    case AbbrMap.find (forward, abbr) of
		NONE => Iri.fromString curie
	      | SOME expansion =>
                Iri.addSuffix (expansion,
                               WdString.fromUtf8 (String.concatWith ":" rest))

    fun abbreviate ((_, reverse, trie) : t, iri) =
      let val prefix = IriTrie.prefixOf (trie, iri)
	  open WdString
	  fun dropPrefix (iri, n) =
  	      implode (List.drop (explode (Iri.toWideString iri), n))
      in
	  if Iri.isEmpty prefix
	  then NONE
	  else
	      case IriMap.find (reverse, prefix) of
		  NONE => raise Fail ("internal error: prefix found in trie " ^
                                      "but not in reverse map")
		| SOME abbr =>
	          SOME (abbr, toUtf8 (dropPrefix (iri, Iri.size prefix)))
      end
		   
end
