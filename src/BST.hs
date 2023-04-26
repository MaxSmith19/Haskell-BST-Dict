module BST (
    BST(..),
    emptyBST,
    insert,
    treeLookup,
    treeToList,
    remove,
    removeIf,
    findMin,
    treeHeight,
    rotateLeft,
    rotateRight
) where

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

-- create an empty tree
emptyBST :: BST key value
emptyBST = Empty
-- as we defined a binary search tree as either Empty or Node,
-- we can create an empty binary search tree by simply returning an "Empty" tree

--insert an entry 
insert :: (Ord key) => key -> value -> BST key value -> BST key value
insert k v Empty = Node k v Empty Empty -- if empty, create a new node
insert k v (Node keyToAdd valueToAdd left right) -- if not empty, add to the tree
    | k == keyToAdd = Node keyToAdd v left right
    | k < keyToAdd = Node keyToAdd valueToAdd (insert k v left) right
    | k > keyToAdd = Node keyToAdd valueToAdd left (insert k v right)

--lookup the item by specifying a key
treeLookup :: (Ord key) => key -> BST key value -> Bool
treeLookup k Empty = False -- if empty, return false
treeLookup k (Node keyToFind valueToFind left right) 
    | k == keyToFind = True
    | k < keyToFind = treeLookup k left
    | k > keyToFind = treeLookup k right

-- produce an ordered list of all entries (as pairs in a list data structure)
treeToList :: (Ord key) => BST key value -> [(key, value)]
treeToList Empty = []
treeToList (Node key value left right) = 
    let 
        leftList = treeToList left
        rightList = treeToList right
    in
        leftList ++ [(key, value)] ++ rightList
        
-- remove an entry from the tree by specifying the key
remove :: (Ord key) => key -> BST key value -> BST key value
remove k Empty = Empty
remove k (Node key value left right)
    | k < key = Node key value (remove k left) right
    | k > key = Node key value left (remove k right)
    | otherwise = case (left, right) of
        (Empty, Empty) -> Empty -- if both are empty, return empty
        (Empty, _) -> right -- if left parent is empty, replace it with the right parent
        (_, Empty) -> left -- if right parent is empty, replace it with the left parent
        (_, _) -> Node minKey minValue left (remove minKey right)
            where -- if both are not empty, replace the node with the minimum value of the right subtree
                (minKey, minValue) = findMin right

findMin :: (Ord key) => BST key value -> (key, value)-- find the minimum value of the tree
findMin (Node k v Empty _) = (k, v) -- if the left node is empty, return the current node
findMin (Node _ _ left _) = findMin left -- if the left node is not empty, recursively go to the left node

-- remove all entries that satisfy a specified predicate function
-- like being even or odd
removeIf :: (Ord key) => (key -> Bool) -> BST key value -> BST key value
removeIf _ Empty = Empty -- if empty, return empty (base case)
removeIf predicate (Node key value left right) -- if not empty, check if the predicate is true
    | predicate key = remove key (Node key value (removeIf predicate left) (removeIf predicate right)) -- if true, remove the node
    | otherwise = Node key value (removeIf predicate left) (removeIf predicate right) -- if false, keep the node


treeHeight :: BST k v -> Int
treeHeight Empty = 0
treeHeight (Node k v left right) = 1 + max (treeHeight left) (treeHeight right)
-- -1 is used instead of this case so that it contributes to the height of the tree calculation
-- otherwise, the height of the tree would be 1 less than it actually is

-- Rotate the binary search tree to the left
rotateLeft :: BST k v -> BST k v
rotateLeft Empty = Empty
rotateLeft (Node k v left (Node rotatedK rotatedV rotatedL rotatedR)) = Node rotatedK rotatedV (Node k v left rotatedL) rotatedR
rotateLeft (Node k v left right) = Node k v (rotateLeft left) right

rotateRight :: BST k v -> BST k v
rotateRight Empty = Empty

-- example tree
-- treeWithNodes :: BST Int String
-- treeWithNodes = Node 2 "B"
--                 (Node 1 "A" Empty Empty) -- left
--                 (Node 3 "C" Empty Empty) -- right
