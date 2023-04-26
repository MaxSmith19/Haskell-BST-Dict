module Spec where

import Test.HUnit
import Test.QuickCheck
import BST
    ( emptyBST,
      insert,
      remove,
      removeIf,
      treeLookup,
      treeToList,
      treeHeight,
      rotateLeft,
      rotateRight,
      BST(..) )

isEven :: Int -> Bool
isEven k = k `mod` 2 == 0
isOdd :: Int -> Bool
isOdd k = k `mod` 2 /= 0

treeWithNodes:: BST Int String
treeWithNodes = Node 2 "B"
                        (Node 1 "A" Empty Empty) -- left
                        Empty -- right

largerTreeWithNodes :: BST Int String
largerTreeWithNodes = Node 4 "D"
                        (Node 2 "B" 
                            (Node 1 "A" Empty Empty) -- left
                            (Node 3 "C" Empty Empty) -- right
                        )
                        (Node 7 "G" 
                            (Node 5 "F" Empty Empty) -- left
                            Empty -- right
                        )

hugeTreeWithNodes :: BST Int String
hugeTreeWithNodes = Node 6 "Eren"
                        (Node 3 "Claire"
                            (Node 2 "Ben"
                                (Node 1 "Ashleigh" Empty Empty) -- left
                                Empty -- right
                            )
                            (Node 4 "Dave" Empty Empty) -- right
                        )
                        (Node 9 "Henry"
                            (Node 8 "Gertrude"
                                (Node 7 "Frank" Empty Empty) -- left
                                Empty -- right
                            )
                            (Node 10 "Italy" Empty Empty) -- right
                        )
                    


main :: IO()
main = do
    _ <- runTestTT constructTests
    _ <- runTestTT insertionTests
    _ <- runTestTT orderedTests
    _ <- runTestTT removeTests
    _ <- runTestTT removeIfTests
    _ <- runTestTT heightTests
    _ <- runTestTT rotateTests
    return ()


constructTests :: Test
constructTests = TestList [
    testEmptyTree,
    testTreeWithNodes
    ]

insertionTests :: Test
insertionTests = TestList [
    testTreeInsertion,
    testTreeInsertionRoot,
    testTreeInsertionDeeper,
    testTreeInsertDuplicate,
    testTreeInsertNegativeKey
    ]
lookupTests :: Test
lookupTests = TestList [
    testTreeLookup,
    testTreeFalseLookup,
    testTreeEmptyLookup
    ]
    
orderedTests :: Test
orderedTests = TestList [
    testTreeOrderedList,
    testTreeOrderedListLarger
    ]

removeTests :: Test
removeTests = TestList [
    testTreeNodeRemoval,
    testTreeNodeRemoveLeaf,
    testTreeNodeRemoveOneChild,
    testTreeNodeRemoveParentOfTwo,
    testTreeRemoveRootNode
    ]
removeIfTests :: Test
removeIfTests = TestList [
    testTreeRemoveIfOdd,
    testTreeRemoveIfEven,
    testTreeRemoveAll,
    testTreeRemoveNone
    ]

heightTests :: Test
heightTests = TestList [
    testTreeHeight,
    testTreeEmptyHeight,
    testTreeInsertHeight,
    testTreeRemoveHeight,
    testTreeRemoveHeightChange
    ]

rotateTests :: Test
rotateTests = TestList [
    testTreeRotateLeft,
    testTreeRotateRight
    ]
-- CREATING A TREE TESTS
testEmptyTree :: Test
testEmptyTree = TestCase (assertEqual "Creates an empty tree" Empty (emptyBST :: BST Int String))
 
testTreeWithNodes :: Test
testTreeWithNodes = TestCase (assertEqual "Creates a tree with nodes" expectedTree (treeWithNodes :: BST Int String))
    where 
        expectedTree = Node 2 "B"
                        (Node 1 "A" Empty Empty) -- left
                        Empty -- right
-- INSERTION TESTS
testTreeInsertion :: Test
testTreeInsertion = TestCase (assertEqual "Inserts a node into a tree" expectedTree (insertedTree :: BST Int String))
    where 
        insertedTree = insert 3 "C" treeWithNodes 
        
        expectedTree = Node 2 "B"   
                        (Node 1 "A" Empty Empty) -- left
                        (Node 3 "C" Empty Empty) -- right

testTreeInsertionRoot :: Test
testTreeInsertionRoot = TestCase(assertEqual "Inserts the root node into an empty tree" expectedTree(insertedTree :: BST Int String))
    where
        insertedTree = insert 2 "B" emptyBST

        expectedTree = Node 2 "B" Empty Empty

testTreeInsertionDeeper :: Test  
testTreeInsertionDeeper = TestCase(assertEqual "Inserts a node Deeper into a tree" expectedTree(insertedTree :: BST Int String))
    where
        
        insertedTree = insert 6 "newItem" largerTreeWithNodes

        expectedTree = Node 4 "D"
                        (Node 2 "B" 
                            (Node 1 "A" Empty Empty) -- left
                            (Node 3 "C" Empty Empty) -- right
                        )
                        (Node 7 "G" 
                            (Node 5 "F"
                             Empty --left
                            (Node 6 "newItem" Empty Empty) -- right
                            ) -- left
                            Empty
                        )

testTreeInsertDuplicate :: Test
testTreeInsertDuplicate = TestCase(assertEqual "Inserts a duplicate node into a tree" expectedTree(insertedTree :: BST Int String))
    where
        
        insertedTree = insert 2 "newItem" largerTreeWithNodes

        expectedTree = Node 4 "D"
                        (Node 2 "newItem" 
                            (Node 1 "A" Empty Empty) -- left
                            (Node 3 "C" Empty Empty) -- right
                        )
                        (Node 7 "G" 
                            (Node 5 "F" Empty Empty) -- left
                            Empty -- right
                        )

testTreeInsertNegativeKey :: Test
testTreeInsertNegativeKey = TestCase(assertEqual "Inserts a node with a negative key into a tree" expectedTree(insertedTree :: BST Int String))
    where
        
        insertedTree = insert (-1) "newItem" largerTreeWithNodes

        expectedTree = Node 4 "D"
                        (Node 2 "B" 
                            (Node 1 "A" 
                                (Node (-1) "newItem" Empty Empty) -- left
                                Empty -- right
                            )
                            (Node 3 "C" Empty Empty) -- right
                        )
                        (Node 7 "G" 
                            (Node 5 "F" Empty Empty) -- left
                            Empty -- right
                        )
-- LOOKUP TESTS                        
testTreeLookup :: Test
testTreeLookup = TestCase (assertEqual "Looks up a node in a tree" True (treeLookup 4 treeToLookup))
    where
        treeToLookup = Node 2 "B"
                        (Node 1 "A" Empty Empty) -- left
                        (Node 5 "D"
                         (Node 4 "C" Empty Empty)
                         (Node 6 "E" Empty Empty)) -- right

testTreeFalseLookup :: Test
testTreeFalseLookup = TestCase (assertEqual "Looks up a node that is not in the tree" False (treeLookup 20 treeToLookup))
    where
        treeToLookup = Node 2 "B"
                        (Node 1 "A" Empty Empty) -- left
                        (Node 5 "D"
                         (Node 4 "C" Empty Empty)
                         (Node 6 "E" Empty Empty)) -- right

testTreeEmptyLookup :: Test
testTreeEmptyLookup = TestCase (assertEqual "Looks up a node in an empty tree" False (treeLookup 20 emptyBST))

--ORDER TESTS
testTreeOrderedList :: Test
testTreeOrderedList = TestCase (assertEqual "Checks the list of nodes is in order" expectedList (treeToList largerTreeWithNodes))
    where
        expectedList = [(1,"A"),(2,"B"),(3,"C"),(4,"D"),(5,"F"),(7,"G")]

testTreeOrderedListLarger :: Test
testTreeOrderedListLarger = TestCase (assertEqual "Checks the list of nodes is in order" expectedList (treeToList hugeTreeWithNodes))
    where
        expectedList = [(1,"Ashleigh"),(2,"Ben"),(3,"Claire"),(4,"Dave"),(6,"Eren"),(7,"Frank"),(8,"Gertrude"),(9,"Henry"),(10,"Italy")]

--REMOVAL TESTS
testTreeNodeRemoval :: Test
testTreeNodeRemoval = TestCase (assertEqual "Removes a node from a tree" expectedTree (treeToList (remove 1 treeWithNodes)))
    where
        expectedTree = [(2,"B")]

testTreeNodeRemoveLeaf :: Test
testTreeNodeRemoveLeaf = TestCase (assertEqual "Removes a child node from a tree" expectedTree (treeToList (remove 1 hugeTreeWithNodes)))
    where
        expectedTree = [(2,"Ben"),(3,"Claire"),(4,"Dave"),(6,"Eren"),(7,"Frank"),(8,"Gertrude"),(9,"Henry"),(10,"Italy")]

testTreeNodeRemoveOneChild :: Test
testTreeNodeRemoveOneChild = TestCase (assertEqual "Removes a child node from a tree" expectedTree (treeToList (remove 2 hugeTreeWithNodes)))
    where
        expectedTree = [(1,"Ashleigh"),(3,"Claire"),(4,"Dave"),(6,"Eren"),(7,"Frank"),(8,"Gertrude"),(9,"Henry"),(10,"Italy")]

testTreeNodeRemoveParentOfTwo :: Test
testTreeNodeRemoveParentOfTwo = TestCase (assertEqual "Removes a parent node from a tree" expectedTree (treeToList (remove 3 hugeTreeWithNodes)))
    where
        expectedTree = [(1,"Ashleigh"),(2,"Ben"),(4,"Dave"),(6,"Eren"),(7,"Frank"),(8,"Gertrude"),(9,"Henry"),(10,"Italy")]

testTreeRemoveRootNode :: Test
testTreeRemoveRootNode = TestCase (assertEqual "Removes the root node from a tree" expectedTree (treeToList (remove 4 hugeTreeWithNodes)))
    where
        expectedTree = [(1,"Ashleigh"),(2,"Ben"),(3,"Claire"),(6,"Eren"),(7,"Frank"),(8,"Gertrude"),(9,"Henry"),(10,"Italy")]

-- REMOVE IF TESTS
testTreeRemoveIfOdd :: Test
testTreeRemoveIfOdd = TestCase (assertEqual "Removes all nodes with odd keys from a tree" expectedTree (treeToList (removeIf isOdd hugeTreeWithNodes)))
    where
        expectedTree = [(2,"Ben"),(4,"Dave"),(6,"Eren"),(8,"Gertrude"),(10,"Italy")]

testTreeRemoveIfEven :: Test
testTreeRemoveIfEven = TestCase (assertEqual "Removes all nodes with even keys from a tree" expectedTree (treeToList (removeIf isEven hugeTreeWithNodes)))
    where
        expectedTree = [(1,"Ashleigh"),(3,"Claire"),(7,"Frank"),(9,"Henry")]

testTreeRemoveAll :: Test
testTreeRemoveAll = TestCase (assertEqual "Removes all nodes from a tree" expectedTree (treeToList (removeIf (const True) hugeTreeWithNodes)))
    where
        expectedTree = []

testTreeRemoveNone :: Test
testTreeRemoveNone = TestCase (assertEqual "Removes no nodes from a tree" expectedTree (treeToList (removeIf (const False) hugeTreeWithNodes)))
    where
        expectedTree = treeToList hugeTreeWithNodes

-- HEIGHT TESTS
testTreeHeight :: Test
testTreeHeight = TestCase (assertEqual "Checks the height of a tree" expectedHeight (treeHeight hugeTreeWithNodes))
    where
        expectedHeight = 4

testTreeEmptyHeight :: Test
testTreeEmptyHeight = TestCase (assertEqual "Checks the height of an empty tree" expectedHeight (treeHeight emptyBST))
    where
        expectedHeight = 0

testTreeInsertHeight :: Test
testTreeInsertHeight = TestCase (assertEqual "Checks the height of a tree after inserting a node (should not affect height)" expectedHeight (treeHeight (insert 12 "J" insertTree)))
    where
        insertTree = insert 12 "K" hugeTreeWithNodes

        expectedHeight = 4

testTreeRemoveHeight :: Test
testTreeRemoveHeight = TestCase (assertEqual "Checks the height of a tree after removing a node (should not affect height)" expectedHeight (treeHeight (remove 1 removeTree)))
    where
        removeTree = remove 1 hugeTreeWithNodes

        expectedHeight = 4

testTreeRemoveHeightChange :: Test
testTreeRemoveHeightChange = TestCase (assertEqual "Checks the height of a tree after removing a node that changes tree height" expectedHeight (treeHeight (remove 6 hugeTreeWithNodes)))
    where
        expectedHeight = 3

--rotation tests
testTreeRotateLeft :: Test
testTreeRotateLeft = TestCase (assertEqual "Checks the tree is rotated left" expectedTree (rotateLeft largerTreeWithNodes))
    where
        expectedTree = Node 7 "G"
                        (Node 4 "D"
                            (Node 2 "B"
                                (Node 1 "A" Empty Empty)
                                (Node 3 "C" Empty Empty)
                            )
                            (Node 5 "F" Empty Empty)
                        )
                        Empty

testTreeRotateRight :: Test
testTreeRotateRight = TestCase (assertEqual "Checks the tree is rotated right" expectedTree (rotateRight largerTreeWithNodes))
    where
        expectedTree = Node 2 "B" 
                (Node 1 "A" Empty Empty) 
                (Node 4 "D" 
                    (Node 3 "C" Empty Empty)
                    (Node 7 "G" 
                        (Node 5 "F" Empty Empty)
                    Empty))

-- Property Tests
-- propertyTests :: IO ()
-- propertyTests = do
--     quickCheck prop_insertLookup
--     quickCheck prop_insertRemove


-- instance (Ord key, Arbitrary key, Arbitrary value) => Arbitrary (BST key value) where
--     arbitrary = oneof [return Empty, createNode]
--       where
--         createNode = do
--           key <- arbitrary
--           value <- arbitrary
--           left <- arbitrary
--           right <- arbitrary
--           return $ Node key value left right

-- prop_insertLookup :: Int -> String -> BST Int String -> Bool
-- prop_insertLookup k v tree = treeLookup k (insert k v tree)

-- prop_insertRemove :: Int -> String -> BST Int String -> Bool
-- prop_insertRemove k v tree = remove k (insert k v tree) == tree