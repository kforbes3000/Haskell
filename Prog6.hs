{- ##################################
   Kamile Forbes
   Homework 6.
   ################################## -}
module Prog6 where

data Tree1 = Leaf1 Int
        | Node1 Int Tree1 Tree1

{- ##################################
Write a function preorder that takes a tree argument and returns as a list an preorder traversal of the tree.
################################## -}
preorder :: Tree1 -> [Int]
preorder (Leaf1 n) = [n]
preorder (Node1 left r right) = [left] ++ preorder r ++ preorder right 

{- ##################################
 Write a function postorder that takes a tree argument and returns as a list an postorder traversal of the tree.
################################## -}
postorder :: Tree1 -> [Int]
postorder (Leaf1 n) = [n]
postorder (Node1 n2 r n3) =  postorder r ++ postorder n3 ++ [n2]

{- ##################################
Write a function sumPositives that takes a tree argument and returns the sum of positive integers in the tree.
################################## -}
sumPositives :: Tree1 -> Int
sumPositives tree = sum [x | x <- postorder tree, x > 0]

{- ##################################
Write a function countLeaves that returns the number of leaves in the given tree. countLeaves :: Tree1 -> Int
################################## -}
countLeaves :: Tree1 -> Int
countLeaves (Leaf1 n) = 0
countLeaves (Node1 x l r) = 1  + countLeaves l + countLeaves r

{- ##################################
 Write a function depth that returns the depth of a tree. (A tree with only a root node is defined to have depth=0.)
################################## -}
depth :: Tree1 -> Int
depth (Leaf1 n) = 1
depth (Node1 n2 r n3) = 1 + maximum [depth r, depth n3]


data Tree2 a = Leaf2 a
              | Node2 [Tree2 a]

{- ##################################
Write a function occurs that returns whether a given argument is present in a given tree. occurs :: Eq a => a -> Tree2 a -> Bool
################################## -}
occurs :: Eq a => a -> Tree2 a -> Bool
occurs n (Leaf2 x) = n == x
occurs n (Node2 []) = False
occurs n (Node2 (x:xs)) = or [occurs n x, occurs n (Node2 xs)]

{- ##################################
Write a function countInteriorNodes that takes a tree argument and returns the number of interior nodes in the tree.
################################## -}
countInteriorNodes :: Tree2 a -> Int
countInteriorNodes (Leaf2 n) =  0
countInteriorNodes (Node2 []) = 0
countInteriorNodes  (Node2 (x:xs)) = countInteriorNodes x + countInteriorNodes (Node2 xs)

{- ##################################
Write a function sumTree that takes a tree of integers and returns the sum of all integers in the tree
################################## -}
sumTree :: Tree2 Int -> Int
sumTree (Leaf2 n) = n
sumTree (Node2 []) = 0
sumTree (Node2 (x:xs)) = sumTree x + sumTree (Node2 (xs))

{- ##################################
Write a function pre2 that returns a preorder traversal of the nodes in the tree.
pre2 :: Tree2 a -> [a]
################################## -}
pre2 :: Tree2 a -> [a]
pre2 (Leaf2 n) = [n]
pre2 (Node2 []) = []
pre2 (Node2 (x:xs)) =  pre2 (Node2 xs) ++ pre2 x
   
{- ##################################
Write a function depthK that returns all nodes that are at depth k in the tree. (A tree with only a root node is defined to have depth=0.) The order that the nodes are returned does not matter. depthK :: Int -> Tree2 a -> [a]   
################################## -}
depthK :: Int -> Tree2 a -> [a]
depthK 1 (Leaf2 m) = [m] 
depthK 1 (Node2 xs) = []
depthK n (Leaf2 m) = [] 
depthK n (Node2 []) = [] 
depthK n (Node2 (x:xs)) = depthK (n-1) x ++ depthK n (Node2 xs) 