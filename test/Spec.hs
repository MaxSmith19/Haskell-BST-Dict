module Spec where

import Test.HUnit
import Test.QuickCheck
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

import Dictionary
    (Dictionary,
    emptyDict,
    dictInsert,
    dictLookup,
    toList,
    dictRemove,
    dictRemoveIf
     )

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
                    
largeDictionary :: Dictionary Int String
largeDictionary = Node 6 "Eren"
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
    testTreeRemoveHeightChange
    ]

rotateTests :: Test
rotateTests = TestList [
    testTreeRotateLeft,
    testTreeRotateRight,
    testTreeRotateLeftRight,
    testTreeRotateRightLeft
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
        insertedTree = treeInsert 3 "C" treeWithNodes 
        
        expectedTree = Node 2 "B"   
                        (Node 1 "A" Empty Empty) -- left
                        (Node 3 "C" Empty Empty) -- right

testTreeInsertionRoot :: Test
testTreeInsertionRoot = TestCase(assertEqual "Inserts the root node into an empty tree" expectedTree(insertedTree :: BST Int String))
    where
        insertedTree = treeInsert 2 "B" emptyBST

        expectedTree = Node 2 "B" Empty Empty

testTreeInsertionDeeper :: Test  
testTreeInsertionDeeper = TestCase(assertEqual "Inserts a node Deeper into a tree" expectedTree(insertedTree :: BST Int String))
    where
        
        insertedTree = treeInsert 6 "newItem" largerTreeWithNodes

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
        
        insertedTree = treeInsert 2 "newItem" largerTreeWithNodes

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
        
        insertedTree = treeInsert (-1) "newItem" largerTreeWithNodes

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
testTreeInsertHeight = TestCase (assertEqual "Checks the height of a tree after inserting a node (should not affect height)" expectedHeight (treeHeight (treeInsert 12 "J" insertTree)))
    where
        insertTree = treeInsert 12 "K" hugeTreeWithNodes

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

testTreeRotateLeftRight :: Test
testTreeRotateLeftRight = TestCase (assertEqual "Checks the tree is rotated left then right" largerTreeWithNodes (rotateLeft (rotateRight largerTreeWithNodes)))

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

testTreeRotateRightLeft :: Test
testTreeRotateRightLeft = TestCase (assertEqual "Checks the tree is rotated right then left" largerTreeWithNodes (rotateRight (rotateLeft largerTreeWithNodes)))

--Property tests
propertyTests :: IO ()
propertyTests = do
    quickCheck prop_insertLookup
    quickCheck prop_insertRemove
    quickCheck prop_rotateLeftRight


instance (Ord key, Arbitrary key, Arbitrary value) => Arbitrary (BST key value) where
    arbitrary = oneof [return Empty, createNode]
      where
        createNode = do
          key <- arbitrary
          value <- arbitrary
          left <- arbitrary
          right <- arbitrary
          return $ Node key value left right



prop_insertLookup :: Int -> String -> BST Int String -> Bool
prop_insertLookup k v tree = treeLookup k (treeInsert k v tree)
-- treeInsert the key and value into the tree, then look up the key in the tree.

prop_insertRemove :: Int -> String -> BST Int String -> Bool
prop_insertRemove k v tree = remove k (treeInsert k v tree) == tree
-- treeInsert the key and value into the tree, then remove the key from the tree.
-- The result should be the same as the original tree.

-- property test to preserve keys after rotation
prop_rotateLeftRight :: BST Int String -> Bool
prop_rotateLeftRight tree = treeToList tree == treeToList (rotateLeft (rotateRight tree))

-- DICTIONARY TESTS
-- construct a dictionary

dict :: IO ()
dict = do
    _ <- runTestTT dictConstructTests
    _ <- runTestTT dictInsertTests
    _ <- runTestTT dictLookupTests
    _ <- runTestTT dictRemoveTests
    _ <- runTestTT dictionaryRemoveIfTests
    return ()

dictConstructTests :: Test
dictConstructTests = TestList [
    testDictionary,
    testEmptyDictionary
    ]

dictInsertTests :: Test
dictInsertTests = TestList [
    testInsertDictionary,
    testInsertDictionaryDuplicate
    ]

dictLookupTests :: Test
dictLookupTests = TestList [
    testLookupDictionary,
    testEmptyLookupDictionary,
    testDictionaryToList
    ]

dictRemoveTests :: Test
dictRemoveTests = TestList [
    testRemoveDictionary,
    testDictionaryRemoveLeaf,
    testDictionaryRemoveParentOneChild,
    testDictionaryRemoveParentTwoChildren,
    testDictionaryRemoveRoot
    ]

dictionaryRemoveIfTests :: Test
dictionaryRemoveIfTests = TestList [
    testDictionaryRemoveIfOdd,
    testDictionaryRemoveIfEven,
    testDictionaryRemoveAll,
    testDictionaryRemoveNone
    ]
    
--test the dictionary constructor to fail
testDictionary :: Test
testDictionary = TestCase (assertEqual "Checks the dictionary constructor" largeDictionary (largeDictionary))

testEmptyDictionary :: Test
testEmptyDictionary = TestCase (assertEqual "Checks the empty dictionary constructor" Empty (emptyDict :: Dictionary Int String))

-- DICTIONARY INSERT TESTS
-- insert a key and value into a dictionary
testInsertDictionary :: Test
testInsertDictionary = TestCase (assertEqual "Inserts a key and value into a dictionary" expectedDictionary (dictInsert 1 "Ashleigh" emptyDict))
    where
        expectedDictionary = Node 1 "Ashleigh" Empty Empty

testInsertDictionaryDuplicate :: Test
testInsertDictionaryDuplicate = TestCase (assertEqual "Inserts a duplicate key and value into a dictionary" expectedDictionary (dictInsert 1 "Ashleigh" (dictInsert 1 "Ashleigh" emptyDict)))
    where
        expectedDictionary = Node 1 "Ashleigh" Empty Empty

--DICTIONARY LOOKUP TESTS
-- lookup a key in a dictionary
testLookupDictionary :: Test
testLookupDictionary = TestCase (assertEqual "Looks up a key in a dictionary" True (dictLookup 1 largeDictionary))

testEmptyLookupDictionary :: Test
testEmptyLookupDictionary = TestCase (assertEqual "Looks up a key in an empty dictionary" False (dictLookup 1 emptyDict))

testDictionaryToList:: Test
testDictionaryToList = TestCase (assertEqual "Converts a dictionary to a list" expectedList (toList largeDictionary))
    where
        expectedList = [(1,"Ashleigh"),(2,"Ben"),(3,"Claire"),(4,"Dave"),(6,"Eren"),(7,"Frank"),(8,"Gertrude"),(9,"Henry"),(10,"Italy")]

-- DICTIONARY REMOVE TESTS
-- remove a key from a dictionary
testRemoveDictionary :: Test
testRemoveDictionary = TestCase (assertEqual "Removes a key from a dictionary" expectedDictionary (dictRemove 1 largeDictionary))
    where
        expectedDictionary = Node 6 "Eren" 
                        (Node 3 "Claire" 
                        (Node 2 "Ben" Empty Empty)
                        (Node 4 "Dave" Empty Empty)) 
                        (Node 9 "Henry" 
                        (Node 8 "Gertrude"
                            (Node 7 "Frank" Empty Empty) 
                        Empty) 
                        (Node 10 "Italy" Empty Empty))

testDictionaryRemoveLeaf :: Test
testDictionaryRemoveLeaf = TestCase (assertEqual "Removes a leaf node from a dictionary" expectedDictionary (dictRemove 10 largeDictionary))
    where
        expectedDictionary = Node 6 "Eren" (Node 3 "Claire" (Node 2 "Ben" (Node 1 "Ashleigh" Empty Empty) Empty) (Node 4 "Dave" Empty Empty)) (Node 9 "Henry" (Node 8 "Gertrude" (Node 7 "Frank" Empty Empty) Empty) Empty)

testDictionaryRemoveParentOneChild :: Test
testDictionaryRemoveParentOneChild = TestCase (assertEqual "Removes a parent node with one child from a dictionary" expectedDictionary (dictRemove 9 largeDictionary))
    where
        expectedDictionary =  Node 6 "Eren" (Node 3 "Claire" (Node 2 "Ben" (Node 1 "Ashleigh" Empty Empty) Empty) (Node 4 "Dave" Empty Empty)) (Node 10 "Italy" (Node 8 "Gertrude" (Node 7 "Frank" Empty Empty) Empty) Empty)

testDictionaryRemoveParentTwoChildren :: Test
testDictionaryRemoveParentTwoChildren = TestCase (assertEqual "Removes a parent node with two children from a dictionary" expectedDictionary (dictRemove 3 largeDictionary))
    where
        expectedDictionary = Node 6 "Eren" (Node 4 "Dave" (Node 2 "Ben" (Node 1 "Ashleigh" Empty Empty) Empty) Empty) (Node 9 "Henry" (Node 8 "Gertrude" (Node 7 "Frank" Empty Empty) Empty) (Node 10 "Italy" Empty Empty))
    
testDictionaryRemoveRoot :: Test
testDictionaryRemoveRoot = TestCase (assertEqual "Removes the root node from a dictionary" expectedDictionary (dictRemove 6 largeDictionary))
    where
        expectedDictionary = Node 7 "Frank" (Node 3 "Claire" (Node 2 "Ben" (Node 1 "Ashleigh" Empty Empty) Empty) (Node 4 "Dave" Empty Empty)) (Node 9 "Henry" (Node 8 "Gertrude" Empty Empty) (Node 10 "Italy" Empty Empty))

testDictionaryRemoveIfOdd :: Test
testDictionaryRemoveIfOdd = TestCase (assertEqual "Removes all nodes with odd keys from a dictionary" expectedDictionary (removeIf isOdd largeDictionary))
    where
        expectedDictionary = Node 6 "Eren" (Node 4 "Dave" (Node 2 "Ben" Empty Empty) Empty) (Node 10 "Italy" (Node 8 "Gertrude" Empty Empty) Empty)

testDictionaryRemoveIfEven :: Test
testDictionaryRemoveIfEven = TestCase (assertEqual "Removes all nodes with even keys from a dictionary" expectedDictionary (removeIf isEven largeDictionary))
    where
        expectedDictionary = Node 7 "Frank" (Node 3 "Claire" (Node 1 "Ashleigh" Empty Empty) Empty) (Node 9 "Henry" Empty Empty)

testDictionaryRemoveAll :: Test
testDictionaryRemoveAll = TestCase (assertEqual "Removes all nodes from a dictionary" Empty (removeIf (const True) largeDictionary))

testDictionaryRemoveNone :: Test
testDictionaryRemoveNone = TestCase (assertEqual "Removes No nodes from a dictionary" largeDictionary (removeIf (const False) largeDictionary))
