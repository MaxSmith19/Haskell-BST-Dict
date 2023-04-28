module Dictionary(
    Dictionary,
    emptyDict,
    dictInsert,
    dictLookup,
    toList,
    dictRemove,
    dictRemoveIf
) where 

import BST
    ( emptyBST,
      treeInsert,
      remove,
      removeIf,
      treeLookup,
      treeToList,
      treeHeight,
      rotateLeft,
      rotateRight,
      BST(..) )

--create the dict structure
type Dictionary key value = BST key value

emptyDict :: Dictionary key value
emptyDict = emptyBST

--treeInsert an entry
dictInsert :: (Ord key) => key -> value -> Dictionary key value -> Dictionary key value
dictInsert = BST.treeInsert

--lookup the item by specifying a key
dictLookup :: (Ord key) => key -> Dictionary key value -> Bool
dictLookup = BST.treeLookup

-- produce an ordered list of all entries (as pairs in a list data structure)
toList :: (Ord key) => Dictionary key value -> [(key, value)]
toList = BST.treeToList

-- remove an entry from the tree by specifying the key
dictRemove :: (Ord key) => key -> Dictionary key value -> Dictionary key value
dictRemove = BST.remove

-- remove all entries that satisfy a specified predicate function
-- like being even or odd
dictRemoveIf :: (Ord key) => (key -> Bool) -> Dictionary key value -> Dictionary key value
dictRemoveIf = BST.removeIf



