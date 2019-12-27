{- ##################################
   Kamile Forbes
   Homework 7.
   ################################## -}
module Prog7 where

{- ##################################
  Write a function unique that returns the list of elements that occur exactly once in the argument list. 
You must use recursion and not list comprehension. A helper function, or functions, may be useful. 
 ################################## -}
unique :: Eq a => [a] -> [a]
unique [] = []
unique xs = uni_helper xs xs

{- ##################################
  Write a function value1 that evaluates an expression. value1 :: Expr1 -> Int 
   ################################## -}
data Expr1 = Val1 Int
           | Add1 Expr1 Expr1
           | Sub1 Expr1 Expr1

value1 :: Expr1 -> Int
value1 expr = case expr of
            Val1 n -> n
            Add1 (Val1 n) (Val1 m)  -> n+m
            Sub1 (Val1 n) (Val1 m)  -> n-m
            Add1 expr (Val1 n) -> (value1 expr)+n
            Add1 (Val1 n) expr -> n+(value1 expr)
            Sub1 expr (Val1 n) -> (value1 expr)-n
            Sub1 (Val1 n) expr -> n-(value1 expr)
            Add1 exp1 exp2 -> (value1 exp1)+(value1 exp2)
            Sub1 exp1 exp2 -> (value1 exp1)-(value1 exp2)


{- ##################################
  Create a Expr2 type constructor that also supports 
multiplication addition to the int literal, addition, and subtraction. 
   ################################## -}
data Expr2 = Val2 Int
           | Add2 Expr2 Expr2
           | Sub2 Expr2 Expr2
           | Mul2 Expr2 Expr2
           | Div2 Expr2 Expr2

{- ##################################
  Write a function value2 that evaluates an expression, 
but returns division by zero scenario. value2 :: Expr2 -> Maybe Int 
   ################################## -}
value2 :: Expr2 -> Maybe Int
value2 expr = case expr of
    Div2 _ (Val2 0) -> Nothing
    Val2 n -> Just n
    Add2 n m -> value2_add (value2 n) (value2 m)
    Sub2 n m -> value2_subtract (value2 n) (value2 m)
    Mul2 n m -> value2_mult (value2 n) (value2 m)
    Div2 n m -> value2_div (value2 n) (value2 m)

{- ##################################
  Make the Expr2 type an instance of the Show class. Appropriate define the function 
show so that (Add2 (Val2 3) (Val2 4)) returns the string "(3+4)" 
and (Add2 (Val2 3) (Mul2 (Val2 2)(Val2 4))) returns the string "(3+(2*4))". show :: Expr2 -> String 
   ################################## -}
instance Show Expr2 where
    show (Val2 n) = show n
    show n = show1 n

show1 :: Expr2 -> String
show1 (Val2 n) = show n
show1 (Add2 n m) = "(" ++ show1 n ++ ['+'] ++ show1 m ++ ")"
show1 (Sub2 n m) = "(" ++ show1 n ++ ['-'] ++ show1 m ++ ")"
show1 (Mul2 n m) = "(" ++ show1 n ++ ['*'] ++ show1 m ++ ")"
show1 (Div2 n m) = "(" ++ show1 n ++ ['/'] ++ show1 m ++ ")"



{- ##################################
  Write a function piglatinize that returns a word into its piglatin form:
 if it begins with a vowel, add to the end "yay", else move non-vowels to 
the end of the string until a vowel is at the front and then add to the end "ay". 
The word arguments are guaranteed to have a vowel (a, e, i, o, or u) 
and not begin with the letter y. 
   ################################## -}

piglatinize :: String -> String
piglatinize [] = "Empty List"
piglatinize (x:xs)
                  |x == 'a' = (x:xs) ++ "yay"
                  |x == 'e' = (x:xs) ++ "yay"
                  |x == 'i' = (x:xs) ++ "yay"
                  |x == 'o' = (x:xs) ++ "yay"
                  |x == 'u' = (x:xs) ++ "yay"
                  |x == 'A' = (x:xs) ++ "yay"
                  |x == 'E' = (x:xs) ++ "yay"
                  |x == 'I' = (x:xs) ++ "yay"
                  |x == 'O'= (x:xs) ++ "yay"
                  |x == 'U'= (x:xs) ++ "yay"
                  |otherwise = pig_help (x:xs) ++ "ay"

{- ##################################
A tree is balanced if the number of leaves in the left and right subtree of every node differ 
by at most one. Write a function balanced that returns whether a tree is balanced or not. 
balanced :: Tree a -> Bool
   ################################## -}
data Tree a = Leaf a | Node (Tree a) (Tree a)

balanced :: Tree a -> Bool
balanced (Leaf a) = True
balanced (Node left right) = or [cl left == (cl right - 1), 
                                cl right == (cl left - 1), 
                                cl left == cl right]
cl :: Tree a -> Int
cl (Leaf a) = 1
cl (Node n1 n2) = cl n1 + cl n2

{- ##################################
Now we will extend the Expr example above to contain conditional expressions. 
Take everything from Expr2 , and create an Expr3 , like so: 
The If data constructor (If BExpr3 Expr3 Expr3) will evaluate the boolean 
expression (first argument) and will return the value of the second argument if it is true, 
else it will return the third argument. Define the BExpr3 type as the following: 
 ################################## -}
data Expr3 = Val3 Int
            | Add3 Expr3 Expr3
            | Sub3 Expr3 Expr3
            | Mul3 Expr3 Expr3
            | Div3 Expr3 Expr3
            | If BExpr3 Expr3 Expr3

data BExpr3 = BoolLit Bool
            | Or BExpr3 BExpr3
            | EqualTo Expr3 Expr3
            | LessThan Expr3 Expr3

{- ##################################
Write a function bEval :: BExpr3 -> Bool that evaluates instances of the
 above boolean expression. bEval :: BExpr3 -> Bool 
   ################################## -}
bEval :: BExpr3 -> Bool
bEval (BoolLit n) = n
bEval (Or n m) = or [bEval n, bEval m]
bEval (EqualTo n m) = value3 n == value3 m
bEval (LessThan n m) = value3 n < value3 m


{- ##################################
Write a function value3 that evaluates 
an expression. value3 :: Expr3 -> Maybe Int 
   ################################## -}
value3 :: Expr3 -> Maybe Int
value3 expr = case expr of
    Div3 _ (Val3 0) -> Nothing
    Val3 n -> Just n
    Add3 n m -> value2_add (value3 n) (value3 m)
    Sub3 n m -> value2_subtract (value3 n) (value3 m)
    Mul3 n m -> value2_mult (value3 n) (value3 m)
    Div3 n m -> value2_div (value3 n) (value3 m)
    If b n m -> case bEval b of
        True -> value3 n
        False -> value3 m




{- ##################################
  Helper functions for unique
   ################################## -}
uni_helper1 :: Eq a => a -> [a] -> Int
uni_helper1 n [] = 0
uni_helper1 n (x:xs) = case n == x of
            True -> 1 + uni_helper1 n xs
            False -> uni_helper1 n xs

uni_helper :: Eq a => [a] -> [a] -> [a]
uni_helper [] _ = []
uni_helper (x:xs) ys = case uni_helper1 x ys of
                    1 -> x : uni_helper xs ys
                    _ -> uni_helper xs ys

{- ##################################
  Helper function for piglatinize
   ################################## -}
pig_help :: String -> String  
pig_help (x:xs)
              |x == 'a' = (x:xs) 
              |x == 'e' = (x:xs) 
              |x == 'i' = (x:xs) 
              |x == 'o' = (x:xs) 
              |x == 'u' = (x:xs) 
              |x == 'A' = (x:xs) 
              |x == 'E' = (x:xs) 
              |x == 'I' = (x:xs)
              |x == 'O'= (x:xs) 
              |x == 'U'= (x:xs) 
              |otherwise = pig_help (xs ++ [x])

{- ##################################
  Helper functions for value2
   ################################## -}
value2_add :: Maybe Int -> Maybe Int -> Maybe Int
value2_add _ Nothing = Nothing
value2_add Nothing _ = Nothing
value2_add (Just n) (Just m) = Just (n+m)
value2_subtract :: Maybe Int -> Maybe Int -> Maybe Int
value2_subtract _ Nothing = Nothing
value2_subtract Nothing _ = Nothing
value2_subtract (Just n) (Just m) = Just (n-m)
value2_div :: Maybe Int -> Maybe Int -> Maybe Int
value2_div _        Nothing   = Nothing
value2_div Nothing  _         = Nothing
value2_div _        (Just 0)  = Nothing
value2_div (Just n) (Just m)  = Just (div n m)
value2_mult :: Maybe Int -> Maybe Int -> Maybe Int
value2_mult _ Nothing = Nothing
value2_mult Nothing _ = Nothing
value2_mult (Just n) (Just m) = Just (n*m)
