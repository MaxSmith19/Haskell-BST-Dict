module BinarySearchTreeTests where

import Test.HUnit
import BST

treeWithNodes:: BST Int String
treeWithNodes = Node 2 "B"
                        (Node 1 "A" Empty Empty) -- left
                        Empty -- right

largerTreeWithNodes :: BST Int String
largerTreeWithNodes = Node 3 "C"
                        (Node 2 "B" 
                            (Node 1 "A" Empty Empty) -- left
                            Empty -- right
                        )
                        (Node 4 "D" 
                        (Node 5 "F" Empty Empty)
                        (Node 7 "G" Empty Empty)
                        ) -- Right



main :: IO()
main = do
    runTestTT allTests
    return()


allTests :: Test
allTests = TestList [
    testEmptyTree,
    testTreeWithNodes,
    testTreeInsertion,
    testTreeInsertionRoot,
    testTreeInsertionFurther,
    testTreeLookup,
    testTreeFalseLookup
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

testTreeInsertionFurther :: Test  
testTreeInsertionFurther = TestCase(assertEqual "Inserts a node further into a tree" expectedTree(insertedTree :: BST Int String))
    where
        
        insertedTree = insert 6 "newItem" largerTreeWithNodes

        expectedTree = Node 3 "C"
                        (Node 2 "B" 
                            (Node 1 "A" Empty Empty) -- left
                            Empty -- right
                        )
                        (Node 4 "D" 
                        (Node 5 "F" Empty Empty)
                        (Node 7 "G" (Node 6 "newItem" Empty Empty) Empty)
                        ) -- Right
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
