 {- ##################################
   Kamile Forbes
   Homework 5.
   ##################################### -}
module Prog5 where

 {- ##################################
 Write a function saferemove that removes the given item from a set. (The function should be "safe", if the specified argument is not in the set, the function should return Nothing , otherwise return the new set using the Just data constructor.) saferemove :: Char -> Set -> Maybe Set
   ##################################### -}

saferemove :: Char -> Set -> Maybe Set
saferemove _ = error "undefined"


{- ##################################
  Write a function reverse' that reverses a list. You must use a case expression inside of your function definition. You may not use any built-in Haskell functions. reverse' :: [a] -> [a]
   ##################################### -}
reverse' :: [x] -> [x]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

 {- ##################################
   Write a function isPalindrome that returns if some list can be read the same way forward and backward (e.g. "12345" and "madam"). You must use a case expression inside of your function definition. You may not use any built-in Haskell functions. isPalindrome :: String -> Bool
   ##################################### -}
isPalindrome :: String -> Bool
isPalindrome x = reverse' x == x

 {- ##################################
   Write a function safeFindAfter that takes a string and a list of strings, and returns the remainder of list after the given string is found. The function should be "safe", that is returning the
Maybe type. The item is guaranteed to be in the list a maximum of one time. (Hint: what are the two relevant data constructors for this problem?) safeFindAfter :: String -> [String] -> Maybe [String]
   ##################################### -}
safeFindAfter :: String -> [String] -> Maybe [String]
safeFindAfter _ [] = Nothing
safeFindAfter a (b:bs)
                        | b == a = Just bs
                        | otherwise = safeFindAfter a bs

data Set = Set [Char]
         | EmptySet
    deriving Show
 {- ##################################
   Write a function member that checks whether the given item is present in the given set. member :: Char -> Set -> Bool
   ##################################### -}
member :: Char -> Set -> Bool
member a EmptySet = False
member a (Set []) = False
member k (Set (x:xs))
                    | k == x = True
                    | k /= x = member k (Set xs)

{- ##################################
   Write a function size that returns the number of elements in a given set. size :: Set -> Int
   ##################################### -}    
size :: Set -> Int
size EmptySet = 0
size (Set a) = length a

 {- ##################################
   Write a function add that inserts the given item into a set. (If the item is already in the set, simply return the set unmodified.) (Hint: you may want to program a helper function that takes two Sets and merges them into one.) add :: Char -> Set -> Set
   ##################################### -}
add :: Char -> Set -> Set
add z EmptySet = Set [z]
add z (Set a) 
              |length [x | x <- a, x == z] == 1 = Set a
              |otherwise = Set (a ++ [z])

 {- ##################################
   Write a function equal that returns whether two sets are equal. equal :: Set -> Set -> Bool
   ##################################### -}
equal :: Set -> Set -> Bool
equal EmptySet EmptySet = True
equal (Set [a]) EmptySet = False
equal EmptySet (Set [a]) = False
equal (Set []) (Set []) = True
equal (Set [a]) (Set []) = False
equal (Set []) (Set [a]) = False
equal (Set [a]) (Set [b]) = a == b
equal (Set a) (Set b) = length [1 | c <- a, member c (Set b)] == length b

 {- ##################################
   Write a function union that takes two sets and returns the union of both sets. union :: Set -> Set -> Set
   ##################################### -}
union :: Set -> Set -> Set
union EmptySet EmptySet = EmptySet
union EmptySet b = b
union a EmptySet = a
union (Set []) (Set []) = EmptySet
union (Set []) (Set b) = Set b
union (Set a) (Set []) = Set a
union (Set (x:xs)) (Set (y:ys)) = case x == y of
  True -> add x $ union (Set xs) (Set ys)
  False -> Set $ setToList $ add x $ add y $ union (Set xs) (Set ys)
setToList :: Set -> [Char]
setToList (Set x) = x

{- ##################################
   Write a function intersection that takes two sets and returns the intersection of both sets. intersection :: Set -> Set -> Set
   ##################################### -}
intersection :: Set -> Set -> Set
intersection EmptySet EmptySet = EmptySet
intersection EmptySet _ = EmptySet
intersection _ EmptySet = EmptySet
intersection (Set []) (Set [])= EmptySet
intersection (Set []) (Set _) = EmptySet
intersection (Set _) (Set []) = EmptySet
intersection (Set a) (Set b)  = case Set [c | c <- a , member c (Set b)] of
                                Set [] -> EmptySet
                                Set (x:xs) -> Set [c | c <- a , member c (Set b)]

