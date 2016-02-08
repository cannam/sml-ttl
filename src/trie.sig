
signature TRIE = sig

    (*!!! should have type trie as well? check mlton coding standards for comparison *)
    type t
    type entry

    (* Empty trie *)
    val empty : t

    (* Add the given entry, returning a new trie. If the entry is
       already present, the returned trie will be unchanged *)
    (*!!! should be insert for consistency with ORD_SET ? what about other names? *)
    val add : t * entry -> t

    (* Test whether the trie contains the given entry *)
    val contains : t * entry -> bool

    (* Return the trie with the given entry removed. If the entry is
       not present, the returned trie will be unchanged *)
    val remove : t * entry -> t

    (* Fold over all the entries in the trie, in sort order *)
    val foldl : (entry * 'a -> 'a) -> 'a -> t -> 'a

    (* Return a list of all entries in the trie, in sort order *)
    val enumerate : t -> entry list

    (* Fold over all the entries in the trie that have the given
       prefix, in sort order. The prefix does not need to be present as
       an entry in the trie *)
    val foldl_prefix_match : (entry * 'a -> 'a) -> 'a -> (t * entry) -> 'a 

    (* Return a list of all entries in the trie that have the given
       entry as a prefix, in sort order. The prefix does not need to
       be present as an entry in the trie *)
    val prefix_match : t * entry -> entry list

    (* Return the longest prefix of the given value that is present as
       an entry in the trie. The given value does not need to be
       present as an entry in the trie; if it is present, it will be
       its own longest prefix, and so it will be returned. If there is
       no prefix of the given entry in the trie, an empty entry will
       be returned !!! no -- should return option, shouldn't assume in
       api that the entry type admits an empty form *)
    val prefix_of : t * entry -> entry 
    
end

signature LIST_ENTRY_TRIE = sig

    include TRIE

    type element
    type pattern = element option list

    val foldl_pattern_match : (entry * 'a -> 'a) -> 'a -> (t * pattern) -> 'a
             
end
