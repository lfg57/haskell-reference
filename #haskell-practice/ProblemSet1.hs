{-
===============================================================================
PROGRAM:  "ProblemSet1.hs"
CREATED:  May 20, 2018
UPDATED:  Jan 24, 2020
AUTHOR :  Leon F. Guerrero
-------------------------------------------------------------------------------
Haskell problems using list comprehensions
=============================================================================-}




{-
=========
PROBLEM 1
-------------------------------------------------------------------------------
Create a function "multiples_5_7 n".
@params: upper limit n
@retval: a list containing all numbers from 1 to n that are multiples of
         5 and 7
-------------------------------------------------------------------------------
# multiples_5_7 100   ->   [35,70]
-------------------------------------------------------------------------------
SOLUTION-}
multiples_5_7 n = [x | x <- [1..n],
                       x `mod` 5 == 0,
                       x `mod` 7 == 0]



{-
=========
PROBLEM 2 (Generalization of PROBLEM 1)
-------------------------------------------------------------------------------
Create a function "multiples a b n".
@params: a, b, n
@retval: a list containing all numbers from 1 to n that are multiples of a and b
-------------------------------------------------------------------------------
# multiples 5 7 100   ->   [35,70]
# multiples 6 9 100   ->   [18,36,54,72,90]
-------------------------------------------------------------------------------
SOLUTION-}
multiples a b n = [x | x <- [1..n],
                       x `mod` a == 0,
                       x `mod` b == 0]



{-
=========
PROBLEM 3
-------------------------------------------------------------------------------
Create a function "doubleNumbers n".
@params: upper limit n
@retval: a list containing every number from 1 to n, multiplied by 2
-------------------------------------------------------------------------------
# doubleNumbers 10   ->   [2,4,6,8,10,12,14,16,18,20]
-------------------------------------------------------------------------------
SOLUTION-}
doubleNumbers n = [2*x | x <- [1..n]]



{-
=========
PROBLEM 4
-------------------------------------------------------------------------------
Create a function "squareEvens n".
@params: upper limit n
@retval: a list containing the square of every even number from 1 to n
-------------------------------------------------------------------------------
# squareEvens 12   ->   [4,16,36,64,100,144]
-------------------------------------------------------------------------------
SOLUTION-}
squareEvens n = [x^2 | x <- [1..n], even x]



{-
==========
PLAYGROUND
-------------------------------------------------------------------------------
--1
multiples_5_7 100

--2
multiples 5 7 100
multiples 6 9 100

--3
doubleNumbers 10

--4
squareEvens 12
------------------------------------------------------------------------------}
