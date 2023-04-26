module BinarySearchPropertyTests where

import Test.QuickCheck
import BST

instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (BST k v) where
  arbitrary = sized bstGen
    where
      bstGen 0 = return Empty
      bstGen n = do
        key <- arbitrary
        value <- arbitrary
        let smaller = resize (n `div` 2) (bstGen (n - 1))
        left <- suchThat smaller (\tree -> all (\k' -> k' < key) (keys tree))
        right <- suchThat smaller (\tree -> all (\k' -> k' > key) (keys tree))
        return (Node key value left right)

      keys :: BST k v -> [k]
      keys Empty = []
      keys (Node k _ left right) = k : (keys left ++ keys right)

main :: IO()
main = do
  quickCheck prop_insert_lookup
  quickCheck prop_remove_nonexistent
  quickCheck prop_lookup_false

-- Inserting a key-value pair into a tree and then looking it up should return True.
prop_insert_lookup :: Int -> String -> BST Int String -> Bool
prop_insert_lookup k v tree = treeLookup k (insert k v tree)

-- Removing a non-existent key from a tree should not change the tree.
prop_remove_nonexistent :: Int -> BST Int String -> Bool
prop_remove_nonexistent k tree = remove k tree == tree || not (treeLookup k tree)

-- If a key is not in the tree, looking it up should return False.
prop_lookup_false :: Int -> BST Int String -> Bool
prop_lookup_false k tree = not (treeLookup k tree) || treeLookup k (insert k "value" tree)
