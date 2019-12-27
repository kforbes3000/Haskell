{- ##################################
   Kamile Forbes
   Homework 8.
   ################################## -}
module Prog8 where

{- ##################################
  Write a function sumSqNeg that computes the "sum of squares of negatives". You must use one or more higher-order functions: map, filter, foldr.
   ################################## -}
sumSqNeg :: [Int] -> Int
sumSqNeg = foldr (+) 0 . map (^2) . filter (<0)

{- ##################################
   Write a function containing (without any higher order functions) that returns whether each element in the first list is also in the second list.
   ################################## -}
containing :: Eq a => [a] -> [a] -> Bool
containing [] [] = True
containing [] ys = True
containing xs [] = False
containing (x:xs) ys = case elem x ys of
    True -> containing xs ys
    False -> False

{- ##################################
   Write a function total that applies the function (first argument) to every element in the list (second argument) and sums the result. You must use one or more higher-order functions: map, filter, foldr.
   ################################## -}
total :: (Int -> Int) -> [Int] -> Int
total fn = sum . map fn

{- ##################################
   Write a function containing' (with higher order functions) that returns whether each element in the first list is also in the second list. You must use one or more higher-order functions: map, filter, foldr.
   ################################## -}
containing' :: Eq a => [a] -> [a] -> Bool
containing' xs ys = and (map (containingHelp ys) xs)  

containingHelp :: Eq a => [a] -> (a -> Bool)
containingHelp xs = fn 
  where
    fn x = elem x xs

{- ##################################
   Write a function lengths that returns a list of lengths of the given strings. You must use one or more higher-order functions: map, filter, foldr.
   ################################## -}
lengths :: [String] -> [Int]
lengths = map length

{- ##################################
   Write a function product' that returns the product of a nonempty list of numbers. You must use one or more higher-order functions: map, filter, foldr.
   ################################## -}
product' :: Num a => [a] -> a
product' = foldr (*) 1

{- ##################################
   Write a function max' that returns the largest element of a nonempty list. You must use one or more higher-order functions: map, filter, foldr.
   ################################## -}
max' :: Ord a => [a] -> a
max' (x:xs) = foldr max x xs

{- ##################################
   Write a function append' that appends two lists. You must use one or more higher-order functions: map, filter, foldr.
   ################################## -}
append' :: [a] -> [a] -> [a]
append' xs ys = foldr (:) ys xs 

{- ##################################
   Write a function filterFirst that removes the first element from the list (second argument) that does not satisfy a given predicate function (first argument). You must use one or more higher-order functions: map, filter, foldr.
   ################################## -}
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst p xs = reverse $ fst $ foldr f ([],False) $ reverse xs
  where
    f x (ys,True) = (x:ys,True)
    f x (ys,False)
      | p x = (x:ys,False)
      | otherwise = (ys,True)

{- ##################################
   Write a function filterLast that removes the last element from the list (second argument) that does not satisfy a given predicate function (first argument). You must use one or more higher-order functions: map, filter, foldr.
   ################################## -}
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p xs = filterFirst p (reverse xs)
