import Data.Char

{- ##################################
   Kamile Forbes
   Homework 3.
   ################################## -}

module Prog3 where


{-Write a function productLastPart which, only using library functions, returns the product of the last numbers in the list, where n is the first argument to the function.-}
productLastPart :: Int -> [Int] -> Int 
productLastPart n m = product (drop (length m - n) m)

{-Write a function init' that has identical behavior to the init function. In your definition, you may only use the standard Haskell functions that operate on lists (except for init ). No recursion or list comprehension allowed.-}
init' :: [Int] -> [Int]
init' m = reverse(tail(reverse m))

{-Write a recursive function init'' that has the same behavior as init' . No standard Haskell
functions or list comprehension allowed.-}
init'':: [Int] -> [Int]
init''[] = error  "this function not for empty list"
init''[m] = []
init''(m:ms) = m: init' ms

{-Write a recursive function elemAt that returns the       item of the list, where the first item is index 1. You may not use any of the standard Haskell functions that operate on lists-}
elemAt :: Int -> [Int] -> Int
elemAt 1 (num:el) = num 
elemAt m (num:el) = elemAt(m-1) el

{-Write a function numTimes that returns the number of times that an element occurs in the list. Use recursion, not a list comprehension.-}
numTimes :: Int -> [Int] -> Int
numTimes _ [] = 0
numTimes x (y:ys)
                |x == y = 1+(numTimes x ys)
                |otherwise = numTimes x ys 

{-Write a function lowerFirstLetter that lowercases the first and only first letter of a string.-}
lowerFirstLetter :: String -> String
lowerFirstLetter [] = []
lowerFirstLetter (x:xs) = toLower x:xs


{-Write a function nestedParens that takes a string argument and returns true if it is a nesting of
zero or more pairs of parentheses, e.g. "((()))".-}


nestedParens :: String -> Bool
nestedParens m
              | length m  == 0 = True
              | head m == '(' && head(reverse m) == ')' || head m == ')' && head(reverse m) == '('  = nestedParens (init(tail m))
              | head m /= '(' && head (reverse m) /= ')' = False
              | head m == '(' && head (reverse m) == '(' = False
              | head m == ')' && head (reverse m) == ')' = False
              | head m == ')' && head (reverse m) == '(' = False
              | length m `mod` 2 /=0 = False
              | otherwise = True


{-Write a function triads that generates a list of integer triples, such that and.(Hint: use a list comprehension with multiple generators-}

-- helper function for triads
gcd' :: Int -> Int -> Int
gcd' a b
        | b == 0 = a
        | otherwise = gcd' b (a `mod` b)

triads :: Int -> [(Int, Int, Int)]
triads n = [(x, y, z) |
                x <- [1..n],
                y <- [1..n],
                z <- [1..n],
                z <= n,
                x*x + y*y == z*z,
                x <= y,
                y <= z,
                gcd' x y == 1,
                gcd' y z == 1,
                gcd' x z == 1] 

{-Write a function iSort' that uses insertion sort to sort a list of triples (Float, Int, String) where only the second element (the Int part of the triple) is to be considered during the sorting process.-}

-- helper functions for iSort'
insert :: (Float, Int, String) -> [(Float, Int, String)] -> [(Float, Int, String)]
insert x [] = [x]
insert (x,y,z) ((a,b,c):xy) 
                          | y < b     = (x,y,z):(a,b,c):xy
                          | otherwise = (a,b,c):insert (x,y,z) xy

iSort' :: [(Float, Int, String)] -> [(Float, Int, String)]
iSort' []     = []
iSort' (a:bc) = insert a (iSort' bc)

{-Write a function merge that takes two sorted lists (decreasing order) and merges them into a single sorted list (decreasing order).-}

merge :: [Int] -> [Int] ->[Int]
merge [] y = y 
merge x [] = x
merge (x:xs) (y:ys)
                  |(x>y) = x:merge xs (y:ys)
                  |otherwise = y: merge (x:xs) ys

