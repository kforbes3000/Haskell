{- ##################################
   KAMILE FORBES
   Homework 4.
   ################################## -}

module Prog4 where

--Write a function morerecent that takes two dates and returns whichever one is more recent.
morerecent:: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
morerecent (a,b,c) (x,y,z)
                          |c > z = (a,b,c)
                          |a > x && c == z = (a,b,c)
                          |a == x && b > y && c == z = (a,b,c)
                          |otherwise = (x,y,z)

--Write a function numInMonth that takes a list of dates and a month and returns how many dates
--in the list match the given month.
numInMonth :: [(Int,Int,Int)] -> Int -> Int
numInMonth list dates = length [1 | (x,y,z) <- list, dates == x ]

--Write a function datesInMonth that takes a list of dates and a month and returns a list of dates
--hat match the given month.
datesInMonth :: [(Int, Int, Int)] -> Int -> [(Int,Int,Int)]
datesInMonth list month = [(x,y,z) | (x,y,z) <- list, month == x ]

--Write a function month2Str that takes a date and returns its month as a string. 
--Example: 10/10/19 should return "October".
month2Str::(Int, Int, Int) -> String
month2Str (x,y,z) = month_to_string x 

--Write a function date2Str that takes a date and returns its string equivalent 
--in the form "February 23, 2018". (Hint: what is the operator for concatenating strings?
--Hint: look up how to convert an Int to a String.)
date2Str :: (Int, Int, Int) -> String
date2Str (x,y,z) = (month_to_string x)++ " " ++ show y ++ ", " ++ show z

--helper function for month and date to string 
month_to_string :: Int -> String
month_to_string month
  | month == 1      = "January"
  | month == 2      = "February"
  | month == 3      = "March"
  | month == 4      = "April"
  | month == 5      = "May"
  | month == 6      = "June"
  | month == 7      = "July"
  | month == 8      = "August"
  | month == 9      = "September"
  | month == 10     = "October"
  | month == 11     = "November"
  | month == 12     = "December"
  | otherwise       = error "Date not available"

--What a function monthLookup that takes a numeric day in the calendar year (between 1 and 365) 
--and returns the number of the month that day is in (excluding leap years), where January is month 1.
monthLookup :: Int -> Int
monthLookup a 
            |a <= 31  = 1
            |a <= 59  = 2
            |a <= 90  = 3
            |a <= 120 = 4
            |a <= 151 = 5
            |a <= 181 = 6
            |a <= 212 = 7
            |a <= 243 = 8
            |a <= 273 = 9
            |a <= 304 = 10
            |a <= 334 = 11
            |a <= 365 = 12
            |otherwise = error "unexpected" 

--Write a function monthRange that takes two numeric days (from previous problem) and
--returns an integer list of the months between those dates (inclusive), e.g.: 
--monthRange 23 101 should return [1,2,3,4]. If the second argument is earlier than the
--first argument, return the empty list.
monthRange ::Int -> Int -> [Int]
monthRange a b = [monthLookup a .. monthLookup b]

--Write a function validDate that takes a date and returns whether it is valid 
--(e.g. November 31 is not valid). Do not be concerned about leap years.
validDate :: (Int, Int, Int) -> Bool
validDate (x,y,z) 
                |(x == 1 || x == 3 || x == 5 || x == 7 || x == 8 || x == 10 || x == 12)  && (y <= 31) = True
                |(x == 4 || x == 6 || x == 9 || x == 11) && (y <= 30) = True
                |x == 2 && y==29 && (mod z 400 == 0 || mod z 4 == 0 && mod z 100 /= 0) = True
                |otherwise   = False 
                
{- ##################################
  Write a function validLeapDate that takes a date and returns whether it is a leap date, that is exactly February 29th on a leap year. (Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100.)
   ################################## -}

validLeapDate :: (Int, Int, Int) -> Bool
validLeapDate (x,y,z)
                    |x == 2 && y==29 && (mod z 400 == 0 || mod z 4 == 0 && mod z 100 /= 0) = True
                    |otherwise   = False


{- ##################################
   Write a function season that takes a date and returns the season that the date is in, where the seasons are "Winter", "Spring", "Summer", and "Fall"
   ################################## -}
season :: (Int, Int, Int) -> String
season (x,y,z) 
              |x == 3 && y >= 20 || x == 6  && y <= 21 || x > 3 && x < 6 = "Spring"
              |x == 6 && y >= 21 || x == 9  && y <= 22 || x > 6 && x < 9 = "Summer"
              |x == 9 && y >= 22 || x == 12 && y <= 21 || x > 9 && x < 12  = "Fall"
              |otherwise = "Winter"
