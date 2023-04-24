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
data BST key value = Empty | Node key value (BST key value) (BST key value) deriving (Show, Eq)
-- The BST can either be created with nothing (Empty)
-- or it can be created with a key, value, and two BSTs (Node)
-- deriving is a method of polymorphism, 
-- it allows us to use the same function for different types, we want to be able to compare two BSTs

-- --defining the empty binary search tree
emptyBST :: BST key value
emptyBST = Empty
-- as we defined a binary search tree as either Empty or Node,
-- we can create an empty binary search tree by simply returning an "Empty" tree

--insert an entry 
insert :: (Ord key) => key -> value -> BST key value -> BST key value



-- example tree
-- treeWithNodes :: BST Int String
-- treeWithNodes = Node 2 "B"
--                 (Node 1 "A" Empty Empty) -- left
--                 (Node 3 "C" Empty Empty) -- right
