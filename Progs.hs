module Progs where

import Pear
import Prelude hiding (GT, LT)

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
