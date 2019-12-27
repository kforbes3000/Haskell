
{- ##################################
   Kamile Forbes
   Homework 1.
   ################################## -}
module Prog1 where



import Data.Char

isSingleDigit :: Integer -> Bool
isSingleDigit user_input
            |user_input > (-10) && user_input < 10 = True 
            |otherwise = False


dividesEvenly :: Int -> Int -> Bool
dividesEvenly user_input user_input_2 
            |mod user_input user_input_2 == 0 = True
            |otherwise = False



middle :: Integer -> Integer -> Integer -> Integer
middle  a b c
      |a>c && a<b || a>b && a<c = a
      |b>a && b<c || b>c && b<a = b 
      |c>b && c<a || c>a && c<b = c
      | a == c    || c == b     = c
      | a == b                  = b
      
triangleArea :: Integer -> Integer -> Float
triangleArea a b = (fromIntegral a * fromIntegral b) * (0.5)

floorDecimal :: Float-> Float
floorDecimal a = fromIntegral(floor a)

isNotALetter :: Char -> Bool
isNotALetter a 
              |isLetter a == True = True
              |otherwise = False

nand :: Bool -> Bool -> Bool
nand a b
        | a == True && b == True = False
        | a == False && b == False = True
        | a == True && b== False =False
        | a == False && b== True =False
        

letterGrade :: Integer -> String 
letterGrade grade 
                | grade >=0  && grade <=59 = "F"
                | grade >=60 && grade <=62 = "D-"
                | grade >=63 && grade <=66 = "D"
                | grade >=67 && grade <=69 = "D+"
                | grade >=70 && grade <=72 = "C-"
                | grade >=73 && grade <=76 = "C"
                | grade >=77 && grade <=79 = "C+"
                | grade >=80 && grade <=82 = "B-"
                | grade >=83 && grade <=86 = "B"
                | grade >=87 && grade <=89 = "B+"
                | grade >=90 && grade <=92 = "A-"
                | grade >=93 && grade <=100 = "A"


averageThree :: Integer ->Integer->Integer->Float
averageThree a b c  = (fromIntegral a +fromIntegral b + fromIntegral c)/3
      

howManyBelowAverage :: Integer -> Integer -> Integer -> Integer
howManyBelowAverage num num1 num2
                            |fromIntegral num  < averageThree num num1 num2 && fromIntegral num1 < averageThree num num1 num2  && fromIntegral num2  < averageThree num num1 num2 = 3
                            |fromIntegral num  < averageThree num num1 num2 && fromIntegral num1 < averageThree num num1 num2 ||
                            fromIntegral num  < averageThree num num1 num2 && fromIntegral num2 < averageThree num num1 num2 ||
                            fromIntegral num1  < averageThree num num1 num2 && fromIntegral num2 < averageThree num num1 num2 = 2
                            |fromIntegral num  < averageThree num num1 num2 || fromIntegral num1 < averageThree num num1 num2 || fromIntegral num2 < averageThree num num1 num2 =1
                            |otherwise = 0
