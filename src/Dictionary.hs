module Dictionary(
    Dictionary,
    emptyDict
) where 

import BST
-- A dictionary data structure contains a collection of key/item pairs, such that each key occurs at-most once,
-- but the same item may occur several times. Key/item pairs are known as entries.

--create the dict structure
type Dictionary key value = BST key value

emptyDict :: Dictionary key value
emptyDict = emptyBST

