module BST where

-- A dictionary data structure contains a collection of key/item pairs, such that each key occurs at-most once,
-- but the same item may occur several times. Key/item pairs are known as entries.

-- Four basic dictionary operations are:

--     create an empty dictionary;
--     insert an entry;
--     lookup the item by specifying a key;
--     produce an ordered list of all entries (as pairs in a list data structure).

-- More advanced dictionary operations are:

--     remove an entry by specifying the key;
--     remove all entries that satisfy a specified predicate function


-- defining the Binary search tree data structure
data BST k v = Empty | Node k v (BST k v) (BST k v) deriving (Show, Eq)
-- The BST can either be created with nothing (Empty)
-- or it can be created with a key, value, and two BSTs (Node)

-- --defining the empty binary search tree
emptyBST :: BST k v
emptyBST = Empty