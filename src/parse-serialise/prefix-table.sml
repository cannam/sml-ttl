
structure IriTrie :> TRIE where type entry = Iri.t = struct

    structure WordListTrie = ListEntryTrieFn(struct
				              type t = word
				              val compare = Word.compare
				              end)

    type t = WordListTrie.t
    type trie = t
    type entry = Iri.t

    val empty = WordListTrie.empty

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

    fun remove_with (remover, map, key) =
	case remover (map, key) of (map', _) => map' handle NotFound => map

    fun remove (table as (forward, reverse, trie) : t, abbr) =
	case AbbrMap.find (forward, abbr) of
	    NONE => table
	  | SOME expansion =>
	    (remove_with (AbbrMap.remove, forward, abbr),
             remove_with (IriMap.remove, reverse, expansion),
	     IriTrie.remove (trie, expansion))

    fun add ((forward, reverse, trie) : t, (abbr, expansion)) =
	(AbbrMap.insert (forward, abbr, expansion),
	 IriMap.insert (reverse, expansion, abbr),
	 IriTrie.add (trie, expansion))

    fun from_prefixes prefixes =
        List.foldl (fn ((abbr, e), tab) => add (tab, (abbr, e))) empty prefixes
                    
    fun contains (table : t, abbr) =
	isSome (AbbrMap.find (#1 table, abbr))
		    
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
	  fun drop_prefix (iri, n) =
  	      implode (List.drop (explode (Iri.toWideString iri), n))
      in
	  if Iri.isEmpty prefix
	  then NONE
	  else
	      case IriMap.find (reverse, prefix) of
		  NONE => raise Fail ("internal error: prefix found in trie " ^
                                      "but not in reverse map")
		| SOME abbr =>
	          SOME (abbr, toUtf8 (drop_prefix (iri, Iri.size prefix)))
      end
		   
end
