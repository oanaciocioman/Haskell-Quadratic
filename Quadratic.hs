module Quadratic where

import Data.Char (ord, chr)

quad :: Int -> Int -> Int -> Int -> Int
-- Returns evaluated quadratic expression.
quad a b c x
 = a * x^2 + b * x + c

quadIsZero :: Int -> Int -> Int -> Int -> Bool
-- Returns True if a quadratic expression evaluates to zero;
-- False otherwise
quadIsZero a b c x
 = quad a b c x == 0

quadraticSolver :: Float -> Float -> Float -> (Float,Float)
-- Returns the two roots of a quadratic equation with
-- coefficients a, b, c
quadraticSolver a b c = (x,y)
  where
   x = (-b + d) / n
   y = (-b - d) / n
   d = sqrt (b ^ 2 - 4 * a * c)
   n = 2 * a

realRoots :: Float -> Float -> Float -> Bool
-- Returns True if the quadratic equation has real roots;
-- False otherwise
realRoots a b c = b^2 - 4 * a * c >= 0.0

bigger, smaller :: Int -> Int -> Int
-- Returns first argument if it is larger than the second, the second argument
-- otherwise
bigger x y   | x > y = x
             | True  = y

bigger2, smaller2 :: Int -> Int -> Int
--Another implementation of these functions

bigger2 x y = if x > y then x else y

smaller2 x y = if x < y then x else y

-- Opposite of bigger
smaller x y | x > y = y
            | True  = x

biggestOf3, smallestOf3 :: Int -> Int -> Int -> Int
-- Returns the largest/smallest of three Ints
biggestOf3 a b c
 = bigger (bigger a b) c

-- Ditto smallest of three
smallestOf3 a b c
 = smaller (smaller a b) c

biggest2Of3, smallest2Of3 :: Int -> Int -> Int -> Int
-- Another implementation for these functions
biggest2Of3 a b c
 | a >= b && b >= c = a
 | a >= c && c >= b = a
 | b >= a && a >= c = b
 | b >= c && c >= a = b
 | c >= a && a >= b = c
 | c >= b && b >= a = c

smallest2Of3 a b c
 | a >= b && b >= c = c
 | a >= c && c >= b = b
 | b >= a && a >= c = c
 | b >= c && c >= a = a
 | c >= a && a >= b = b
 | c >= b && b >= a = a

isADigit :: Char -> Bool
-- Returns True if the character represents a digit '0'..'9';
-- False otherwise
isADigit c
 = ord c >= ord '0' && ord c <= ord '9'

-- False otherwise
isAlphabetic :: Char -> Bool
-- Returns True if the character represents an alphabetic
-- character either in the range 'a'..'z' or in the range 'A'..'Z';
isAlphabetic c
 = ord c >= ord 'a' && ord c <= ord 'z' || ord c >= ord 'A' && ord c <= ord 'Z'

digitCharToInt :: Char -> Int
-- Returns the integer [0..9] corresponding to the given character.
digitCharToInt c
 = ord c - ord '0'

toUpperCase :: Char -> Char
-- Returns the upper case character corresponding to the input.
-- Uses guards by way of variety.
toUpperCase c | ord c >= ord 'a' && ord c <= ord 'z' = chr (ord c + ord 'A' - ord 'a')
              | otherwise                            = c
                                                                                                                    
