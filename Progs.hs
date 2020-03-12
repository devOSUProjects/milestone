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

--Example use of for loop. adds 2 to test 5 times resulting in 10
ex6 :: [Stmt]
ex6 = 
      [
         Set "test" (int 0),
         for (int 5) (Mutate "test" (Add (Get "test") (int 2)))
      ]
--Example of using lists
ex7 :: [Stmt]
ex7 = 
      [
         Set "test" (list [Ival 1, Sval "testing strings", Bval False]),
         Set "anothertest" (Index "test" (int (1))),
         Set "test2" (list [Ival 1, Ival 2, Ival 3]),
         Set "NewList" (ConcatLists (Get "test") (Get "test2")),
         Set "??????" (GetListInts (Get "NewList")),
         Set "onlystrings" (GetListStrings (Get "NewList"))
      ]

--Example of using Not expression
ex8 :: [Stmt]
ex8 = 
      [
         Set "x" (int (2)),
         Set "y" (int (3)),
         If (Not (GT (Get "x") (Get "y")))
         (Set "z" (string "Test successful"))
         (Set "z" (string "Test unsuccesful"))
      ]



goodexample1 :: [Stmt]
goodexample1 =
               [
                  Set "goodexample1" (int 2),
                  for (int 19) (Mutate "goodexample1" (Mul (Get "goodexample1") (int 2)))
               ]

goodexample2 :: [Stmt]
goodexample2 =
               [
                  Set "String1" (string "We "),
                  Set "String2" (string "Love "),
                  Set "String3" (string "Our "),
                  Set "String4" (string "Grading "),
                  Set "String5" (string "Ta."),
                  Set "x" (Cat (Get "String4") (Get "String5")),
                  Set "y" (Cat (Get "String2") (Get "String3")),
                  Set "z" (Cat (Get "y") (Get "x")),
                  Set "finaloutput" (Cat (Get "String1") (Get "z"))
               ]
--
--Won't divide by zero
badexample :: [Stmt]
badexample = 
              [
                  Set "baz" (Div (int 19) (int 0))
              ]
