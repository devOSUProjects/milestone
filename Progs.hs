module Progs where

import Pear
import Prelude hiding (GT, LT)
import Libraries

--String Examples
ex1 :: [Stmt]
ex1 = 
      [
         Set "blah" (int 33),
         If (GT (int 7) (int 8)) 
         (Set "Hollyfield" (Add (int 1) (Get "blah")))
         (Set "Hollyfield" (Add (int 19) (int 6)))
      ]

--String Examples
ex2 :: [Stmt]
ex2 = 
      [
         Set "String1" (string "World"),
         Set "String2" (string "World"),
         Set "String3" (Cat (Get "String1")(Get "String2")),
         Set "WordCount" (WC (Get "String3"))
      ]

--Error example
ex3 :: [Stmt]
ex3 = 
      [
         Set "hey" (int 3),
         Set "hey" (int 3)
      ]

--Function example
ex4 :: [Stmt]
ex4 = 
      [
          Set "hello kitty" (int 3),
          Deffunc "func1" "param1" (Prog [Mutate "hello kitty" (Add (Get "param1") (int 4))]),
          Call "func1" (int 3),
          Call "func1" (int 8)
      ]
--Won't divide by zero
ex5 :: [Stmt]
ex5 = 
      [
          Set "baz" (Div (int 19) (int 0))
      ]

--Example use of for loop. adds 2 to test 5 times resulting in 10
ex6 :: [Stmt]
ex6 = 
      [
         Set "test" (int 0),
         for (int 5) (Mutate "test" (Add (Get "test") (int 2)))
      ]
