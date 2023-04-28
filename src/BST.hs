module BST (
    BST(..),
    emptyBST,
    treeInsert,
    treeLookup,
    treeToList,
    remove,
    removeIf,
    treeHeight,
    rotateLeft,
    rotateRight
) where

-- A dictionary data structure contains a collection of key/item pairs, such that each key occurs at-most once,
-- but the same item may occur several times. Key/item pairs are known as entries.

-- Four basic dictionary operations are:

--     create an empty dictionary;
--     treeInsert an entry;
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

treeInsert :: (Ord key) => key -> value -> BST key value -> BST key value
treeInsert key' value' Empty = Node key' value' Empty Empty
treeInsert key' value' (Node keyToAdd valueToAdd left right)
    | key' == keyToAdd = Node keyToAdd value' left right
    | key' < keyToAdd = Node keyToAdd valueToAdd (treeInsert key' value' left) right
    | key' > keyToAdd = Node keyToAdd valueToAdd left (treeInsert key' value' right)


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
        (Empty, Empty) -> Empty
        (Empty, _) -> right
        (_, Empty) -> left
        (_, _) -> Node minKey minValue left (remove minKey right)
            where
                (minKey, minValue) = findMin right

findMin :: BST k v -> (k, v) 
findMin (Node k v Empty _) = (k, v) 
findMin (Node _ _ left _) = findMin left 


-- remove all entries that satisfy a specified predicate function
-- like being even or odd
removeIf :: (Ord key) => (key -> Bool) -> BST key value -> BST key value
removeIf _ Empty = Empty
removeIf predicate (Node key value left right)
    | predicate key = remove key (Node key value (removeIf predicate left) (removeIf predicate right))
    | otherwise = Node key value (removeIf predicate left) (removeIf predicate right)



treeHeight :: BST k v -> Int
treeHeight Empty = 0
treeHeight (Node k v left right) = 1 + max (treeHeight left) (treeHeight right)
-- -1 is used instead of this case so that it contributes to the height of the tree calculation
-- otherwise, the height of the tree would be 1 less than it actually is

-- Performs a left rotation on the given binary search tree
rotateLeft :: BST k v -> BST k v
rotateLeft Empty = Empty
-- Left rotation on a node with a non-empty right subtree
rotateLeft (Node k v left (Node rotatedK rotatedV rotatedL rotatedR))  
    = Node rotatedK rotatedV (Node k v left rotatedL) rotatedR 
-- Left rotation on a node with an empty right subtree
rotateLeft (Node k v left right) = Node k v (rotateLeft left) right


-- Performs a right rotation on the given binary search tree
rotateRight :: BST k v -> BST k v
rotateRight Empty = Empty
-- Right rotation on a node with a non-empty left subtree
rotateRight (Node k v (Node rotatedK rotatedV rotatedL rotatedR) right)
     = Node rotatedK rotatedV rotatedL (Node k v rotatedR right)
-- Right rotation on a node with an empty left subtree
rotateRight (Node k v left right) = Node k v left (rotateRight right)



-- example tree
-- treeWithNodes :: BST Int String
-- treeWithNodes = Node 2 "B"
--                 (Node 1 "A" Empty Empty) -- left
--                 (Node 3 "C" Empty Empty) -- right
