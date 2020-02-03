-- Haskell program to crack the Caesar cipher
-- using simple frequency analysis.




--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- INTRODUCTION
--------------------------------------------------------------------------------
-- A cipher is a method for encoding a message by replacing each character of
-- the message by another character. One of the simplest examples is the Caesar
-- cipher, which is said to have been used for military purposes by Julius
-- Caesar. To encode a message, Caesar simply replaced each letter in the 
-- message by the letter three places further down the alphabet, wrapping 
-- around at the end of the alphabet. 

-- For example, the message     haskell is fun
-- would be encoded as          kdvnhoo lv ixq

-- More generally, the shift factor of three used by Caesar can be replaced by
-- any natural number between one and twenty-ﬁve, thereby giving twenty-ﬁve
-- diﬀerent ways to encode a message. 

-- For example, with a shift factor of ten, the original message above would 
-- be encoded as                rkcuovv sc pex

-- The aim of this exam is to write a Haskell program that can automatically
-- decode such messages, by using letter frequencies to determine the most
-- likely shift factor that was used to encode the message.
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- ENCODING AND DECODING
--------------------------------------------------------------------------------
-- Define the following functions:


--01
--------------------------------------------------------------------------------
-- let2nat :: Char → Int
-- Converts a lower-case letter in the range 'a' to 'z' into the corresponding
-- natural number in the range 0 to 25. 
--------------------------------------------------------------------------------
-- Examples>
-- let2nat 'a' -> 0
-- let2nat 'z' -> 25
--------------------------------------------------------------------------------
-- @params: c : lcLetter
-- @retval: n : ASCIIdec
--
let2nat :: Char -> Int
let2nat c = fromEnum c - fromEnum 'a'



--02
--------------------------------------------------------------------------------
-- nat2let :: Int → Char
-- Performs the inverse function to let2nat. 
--------------------------------------------------------------------------------
-- Examples>
-- nat2let 0  -> 'a'
-- nat2let 25 -> 'z'
-- nat2let 27 -> 'b'
--------------------------------------------------------------------------------
-- @params: n : ASCIIdec
-- @retval: c : lcLetter
--
nat2let :: Int -> Char
nat2let n = toEnum ((n `mod` 26) + fromEnum 'a')



--03
--------------------------------------------------------------------------------
-- shift :: Int→Char → Char
-- Uses: let2nat and nat2let.
-- Applies a shift factor in the range 0 to 25 to a lower-case letter in the
-- range 'a' to 'z'. Characters outside this range, such as upper-case letters
-- and punctuation, should be returned unshifted. Take care to ensure that your
-- function wraps around at the end of the alphabet.
--------------------------------------------------------------------------------
-- Examples>
-- shift 3 'h' -> 'k'
-- shift 3 'z' -> 'c'
-- shift 3 'H' -> 'H'
-- shift 1 'z' -> 'a'
--------------------------------------------------------------------------------
-- @params: n c : shiftFactor lcLetter
-- @retval: c   : shifted_lcLetter / unshifted_Char
--
shift :: Int->Char -> Char
shift n c = if (let2nat c >= 0 && let2nat c <= 25) then nat2let (let2nat c + n) 
            else c
            


--04
--------------------------------------------------------------------------------
-- encode :: Int→String → String 
-- Uses: shift.
-- Encodes a string using a given shift factor. 
--------------------------------------------------------------------------------
-- Examples>
-- encode 3 "haskell is fun" -> "kdvnhoo lv ixq"
--------------------------------------------------------------------------------
-- @params: n xs : shiftFactor string
-- @retval: xs   : encoded_String
--
encode :: Int->String -> String
encode n xs = [shift n letter | letter <- xs]



--05
--------------------------------------------------------------------------------
-- decode :: Int→String → String
-- Performs the inverse function to encode. 
--------------------------------------------------------------------------------
-- Examples>
-- decode 3 "kdvnhoo lv ixq" -> "haskell is fun"
--------------------------------------------------------------------------------
-- @params: n xs : shiftFactor encoded_String
-- @retval: xs   : decoded_String
--
decode :: Int->String -> String
decode n xs = [shift (-n) letter | letter <- xs]




--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- FREQUENCY ANALYSIS
--------------------------------------------------------------------------------
-- In English text, some letters are used more frequently than others. By
-- analyzing a large volume of text, one can derive the following table of
-- approximate percentage frequencies of the twenty-six letters of alphabet:

-- table :: [Float]
-- table == [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0,
--           0.2, 0.8, 4.0, 2.4,  6.7, 7.5, 1.9, 0.1, 6.0,
--           6.3, 9.1, 2.8, 1.0,  2.4, 0.2, 2.0, 0.1]

-- For example, this table shows that 'e' occurs most often, with a frequency 
-- of 12.7%, while 'q' and 'z' occur least often, with a frequency of 0.1%. 
-- You will need to type this table into your Haskell script.
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- table :: [Float]
-- Approximate percentage frequencies of the 26 letters of the alphabet.
--------------------------------------------------------------------------------table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4,  6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.1, 2.8, 1.0,  2.4, 0.2, 2.0, 0.1]


-- Define the following functions:


--06
--------------------------------------------------------------------------------
-- lowers :: String → Int
-- Calculates the number of lower-case letters in a string. 
--------------------------------------------------------------------------------
-- Examples>
-- lowers "haskell is fun" -> 12
--------------------------------------------------------------------------------
-- @params: xs : string
-- @retval: n  : num_lcLetters
lowers :: String -> Int
lowers xs = length [char | char <- xs, char `elem` ['a'..'z']]



--07
--------------------------------------------------------------------------------
-- count :: Char→String → Int 
-- Calculates the number of occurrences of a given character in a string. 
--------------------------------------------------------------------------------
-- Examples>
-- count 's' "haskell is fun" -> 2
--------------------------------------------------------------------------------
-- @params: c xs : char string
-- @retval: n    : numOccrChar
count :: Char->String -> Int
count c xs = length [char | char <- xs, char == c]



--08
--------------------------------------------------------------------------------
-- percent :: Int→Int → Float 
-- Calculates the percentage of one integer with respect to another, returning
-- the result as a floating-point number. 
-- Note: the library function fromIntegral :: Int → Float converts an integer
-- into the corresponding floating-point number.
--------------------------------------------------------------------------------
-- Examples>
-- percent 2 12 -> 16.6667
--------------------------------------------------------------------------------
-- @params: x y : int1 int2
-- @retval: p   : percentXY
--
percent :: Int->Int -> Float
percent x y = (a / b) * 100
    where a = fromIntegral x :: Float
          b = fromIntegral y :: Float



--09
--------------------------------------------------------------------------------
-- freqs :: String → [Float] 
-- Uses: lowers, count, percent.
-- Returns the list of percentage frequencies of each of the lower-case letters
-- 'a' to 'z' in a string of characters.
--------------------------------------------------------------------------------
-- Examples>
-- freqs "haskell is fun" -> 
-- [8.33333, 0.0, 0.0, 0.0, 8.33333, 8.33333, 0.0, 8.33333,
--  8.33333, 0.0, 8.33333, 16.6667, 0.0, 8.33333, 0.0, 0.0,
--  0.0, 0.0, 16.6667, 0.0, 8.33333, 0.0, 0.0, 0.0, 0.0, 0.0]
--------------------------------------------------------------------------------
-- @params: xs : string
-- @retval: xp : xpercentFreq
freqs :: String -> [Float]
freqs xs = [percent (count x xs) (lowers xs) | x <- ['a'..'z']]



--10
--------------------------------------------------------------------------------
-- rotate :: Int→[a] → [a] 
-- Rotates a list n places to the left, wrapping around at the start of the
-- list, and assuming n is in the range zero to the length of the list. 
--------------------------------------------------------------------------------
-- Examples>
-- rotate 3 "haskell is fun" -> "kell is funhas"
--------------------------------------------------------------------------------
-- @params: n xs : numPlaces list
-- @retval: xs   : rotL_numP_list
rotate :: Int->[a] -> [a]
rotate n xs = drop n (take (length xs + n) (cycle xs))



--11
--------------------------------------------------------------------------------
-- chisqr :: [Float]→[Float ] → Float
-- Calculates the chi square statistic for a list of observed frequencies os
-- with respect to a list of expected frequencies es. 
--------------------------------------------------------------------------------
-- The function is given by:
-- chisqr os es = sum(i=0,n-1) ((os_i - es_i)^2 / es_i)
-- where n is the length of the two lists os and es, and xs_i denotes the ith
-- element of a list xs, counting from zero.
--------------------------------------------------------------------------------
-- Examples>
-- chisqr (freqs "haskell is fun") table -> 202.616
--------------------------------------------------------------------------------
-- @params: os es : (freqs string) table
-- @retval: n     : chiSqrStat
chisqr :: [Float]->[Float] -> Float
chisqr os es = sum [(o-e)^2 / e | (o,e) <- zip os es]



--12
--------------------------------------------------------------------------------
-- position :: Eq a ⇒ a→[a ] → Int 
-- Returns the first position (counting from zero) at which a value occurs in
-- a list, assuming that it occurs at least once. 
--------------------------------------------------------------------------------
-- Examples>
-- position 5 [1, 3, 5, 7, 11] -> 2
--------------------------------------------------------------------------------
-- @params: x xs : elem list
-- @retval: n    : indexElemInList
position :: Eq a => a->[a ] -> Int
position x xs = head [pos | (x', pos) <- zip xs [0..], x' == x]




--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- CRACKING THE CIPHER
--------------------------------------------------------------------------------



--13
--------------------------------------------------------------------------------
-- crack :: String → String
-- Uses: functions defined above.
-- Attempts to decode a string by first calculating the letter frequencies in
-- the string, then calculating the chi square value of each rotation (in the
-- range zero to twenty-ﬁve) of this list with respect to the table of expected
-- frequencies, and ﬁnally using the position of the minimum value in this list
-- as the shift factor to decode the original string. 
--------------------------------------------------------------------------------
-- Examples>
-- crack (encode 3 "haskell is fun") -> "haskell is fun"
--------------------------------------------------------------------------------
-- @params: xs [string]
-- @retval: xs [decoded_string]
crack :: String -> String
crack xs = decode decodeShiftFactor xs
    where decodeShiftFactor  = position minChiSqrRotations chiSqrRotations
          minChiSqrRotations = minimum chiSqrRotations
          chiSqrRotations    = [chisqr (rotate x (freqs xs)) table | x <- [0..25]]



--------------------------------------------------------------------------------
-- Note that this method may not be successful if the message is short or
-- contains an unusual distribution of letters. 
--------------------------------------------------------------------------------
-- Examples>
-- crack (encode 3 "graham") ->
--    "nyhoht"
-- crack (encode 3 "the five boxing wizards jump quickly") -> 
--    "dro psfo lyhsxq gsjkbnc tewz aesmuvi"



--------------------------------------------------------------------------------
-- For fun, try out your crack function on the following example:
-- Examples>
-- crack (encode 3 "myxqbkdevkdsyxc yx mywzvodsxq dro ohkw!")




--------------
-- Playground:
--------------
----01
--let2nat 'a'
--let2nat 'z'
--
----02
--nat2let 0
--nat2let 25
--nat2let 27
--
----03
--shift 3 'h'
--shift 3 'z'
--shift 3 'H'
--shift 1 'z'
--
----04
--encode 3 "haskell is fun"
--
----05
--decode 3 "kdvnhoo lv ixq"
--
----06
--lowers "haskell is fun"
--
----07
--count 's' "haskell is fun"
--
----08
--percent 2 12
--
----09
--freqs "haskell is fun"
--
----10
--rotate 3 "haskell is fun"
--
----11
--chisqr (freqs "haskell is fun") table
--
----12
--position 5 [1, 3, 5, 7, 11]
--
----13
--shiftFactor = 3
--message = "haskell is fun"
--crack (encode shiftFactor message)
--
--shiftFactor = 3
--message = "graham"
--crack (encode shiftFactor message)
--
--shiftFactor = 3
--message = "the five boxing wizards jump quickly"
--crack (encode shiftFactor message)
--
--shiftFactor = 3
--message = "myxqbkdevkdsyxc yx mywzvodsxq dro ohkw!"
--crack (encode shiftFactor message)
