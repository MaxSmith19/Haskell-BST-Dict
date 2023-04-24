module BinarySearchTreeTests where

import Test.HUnit
import BST

treeWithNodes:: BST Int String
treeWithNodes = Node 2 "B"
                        (Node 1 "A" Empty Empty) -- left
                        Empty -- right

main :: IO()
main = do
    runTestTT allTests
    return()


allTests :: Test
allTests = TestList [
    testEmptyTree,
    testTreeWithNodes,
    testTreeInsertion
    ]


-- test for a binary search tree
testEmptyTree :: Test
testEmptyTree = TestCase (assertEqual "Creates an empty tree" Empty (emptyBST :: BST Int String))

--test for a tree with nodes 
testTreeWithNodes :: Test
testTreeWithNodes = TestCase (assertEqual "Creates a tree with nodes" expectedTree (treeWithNodes :: BST Int String))
    where 
        expectedTree = Node 2 "B"
                        (Node 1 "A" Empty Empty) -- left
                        Empty -- right

testTreeInsertion :: Test
testTreeInsertion = TestCase (assertEqual "Inserts a node into a tree" expectedTree (insertedTree :: BST Int String))
    where 
        insertedTree = insert 3 "C" treeWithNodes 
        
        expectedTree = Node 2 "B"
                        (Node 1 "A" Empty Empty) -- left
                        (Node 3 "C" Empty Empty) -- right

testTreeLookup :: Test
testTreeLookup = TestCase (assertEqual "Looks up a node in a tree" False (BST.lookup 5 treeWithNodes))

