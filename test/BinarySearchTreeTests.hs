module BinarySearchTreeTests where

import Test.HUnit
import BST

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
    testTreeFalseLookup,
    testTreeOrderedList,
    testTreeOrderedListLarger
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

--test that checks the list of nodes is in order
testTreeOrderedList :: Test
testTreeOrderedList = TestCase (assertEqual "Checks the list of nodes is in order" expectedList (treeToList largerTreeWithNodes))
    where
        expectedList = [(1,"A"),(2,"B"),(3,"C"),(4,"D"),(5,"F"),(7,"G")]

testTreeOrderedListLarger :: Test
testTreeOrderedListLarger = TestCase (assertEqual "Checks the list of nodes is in order" expectedList (treeToList hugeTreeWithNodes))
    where
        expectedList = [(1,"Ashleigh"),(2,"Ben"),(3,"Claire"),(4,"Dave"),(6,"Eren"),(7,"Frank"),(8,"Gertrude"),(9,"Henry"),(10,"Italy")]

testTreeNodeRemoval :: Test
testTreeNodeRemoval = TestCase (assertEqual "Removes a node from a tree" expectedTree (treeToList (remove 1 treeWithNodes)))
    where
        expectedTree = [(2,"B")]