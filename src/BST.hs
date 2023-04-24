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
data BST key value = Empty 
    | Node key value (BST key value) (BST key value) deriving (Show, Eq)
-- The BST can either be created with nothing (Empty)
-- or it can be created with a key, value, and two BSTs (Node)
-- deriving is a method of polymorphism, 
-- it allows us to use the same function for different types, we want to be able to compare two BSTs

-- create an empty dictionary
emptyBST :: BST Int String
emptyBST = Empty
-- as we defined a binary search tree as either Empty or Node,
-- we can create an empty binary search tree by simply returning an "Empty" tree

--insert an entry 
insert :: Int -> String -> BST Int String -> BST Int String
insert k v Empty = Node k v Empty Empty -- if empty, create a new node
insert k v (Node keyToAdd valueToAdd left right) -- if not empty, add to the tree
    | k == keyToAdd = Node keyToAdd v left right
    | k < keyToAdd = Node keyToAdd valueToAdd (insert k v left) right
    | k > keyToAdd = Node keyToAdd valueToAdd left (insert k v right)

--lookup the item by specifying a key
treeLookup :: Int -> BST Int String -> Bool
treeLookup k Empty = False -- if empty, return false
treeLookup k (Node keyToFind valueToFind left right) 
    | k == keyToFind = True
    | k < keyToFind = treeLookup k left
    | k > keyToFind = treeLookup k right

treeToList :: BST Int String -> [(Int, String)]
treeToList Empty = []
treeToList (Node key value left right) = 
    let 
        leftList = treeToList left
        rightList = treeToList right
    in
        leftList ++ [(key, value)] ++ rightList
        

-- example tree
-- treeWithNodes :: BST Int String
-- treeWithNodes = Node 2 "B"
--                 (Node 1 "A" Empty Empty) -- left
--                 (Node 3 "C" Empty Empty) -- right
