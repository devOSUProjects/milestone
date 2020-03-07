module String where

import Pear

--This currently includes operations for manipulating strings for our language
-- Currently we have
--		a. String Compare
--		b. String Concatenation (still trying to clean this one up)
--		c. Number of Words in the String
--		d. Checking for a specific value within a String

--compares two string return true if they are equal, else false
strcmp :: String -> String -> Bool
strcmp [] [] = True
strcmp a [] = False
strcmp [] a = False
strcmp (x:xs) (s:ss) = if ( x == s ) then strcmp xs ss else False

--when given two strings it will concatenate them
strcat :: [[a]] -> [[a]] -> [[a]]
strcat [] _ = []
strcat _ [] = []
strcat (x:xs) (y:ys) = (x ++ y) : strcat xs ys

--Returns the number of words in a word list (String)
wordcount :: String -> Int
wordcount = length . words

--Checks if a given string is contained within the second string.
--The string must be contained exactly as typed in the 2nd string or it will return false.
strscan :: String -> String -> Bool
strscan (_:_) [] = False
strscan xs ys
   | prefix xs ys = True
   | strscan xs (tail ys) = True
   | otherwise = False
   
prefix :: String -> String -> Bool
prefix [] _ = True
prefix (_:_) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys
