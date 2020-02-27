module List where

import While

--
-- This is the List data type and operations for our language 
-- Operations include: 
--     a. creating a list 
--     b. indexing
--     c. concatenation
--     d. looping through
--
data List a = End | Cons a (List a) deriving(Eq, Show)


empty::List a
empty = End


-- Some List Examples
--
a = Cons 1 (Cons 2 (Cons 3 End))
b = Cons 4 (Cons 5 End) 
ex1 = a ~~ b

--
-- Create a List
--
--makeList:: [a] -> List a
--makeList [] = End
--makeList (x:xs) = Cons a (makeList xs) 

--
-- Indexing a List
--

--
-- Concate lists
--
(~~) :: List a -> List a -> List a
End ~~ ys = ys
(Cons x xs) ~~ ys = Cons x  (xs ~~ ys)

--
-- Retrive through a list
--
mapList::(l->a) -> List l -> List a
mapList _ End = End
mapList f (Cons x xs) = Cons (f x) (mapList f xs)  


--
-- Get Length of a List
--
numList::(List a) -> Int
numList End  = 0
numList (Cons _ xs) = 1 + numList xs



