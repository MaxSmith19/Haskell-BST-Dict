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
    testTreeInsertion,
    testTreeLookup,
    testTreeFalseLookup
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
