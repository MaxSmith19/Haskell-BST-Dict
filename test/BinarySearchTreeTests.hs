module BinarySearchTreeTests where

import Test.HUnit
import BST

main :: IO()
main = do
    runTestTT allTests
    return()


allTests :: Test
allTests = TestList [testEmptyTree]


-- test for a binary search tree
testEmptyTree :: Test
testEmptyTree = TestCase (assertEqual "Creates an empty tree" Empty (emptyBST :: BST Int Int))
