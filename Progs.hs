module Progs where

import Pear
import Prelude hiding (GT, LT)

--String Examples
ex1 :: [Stmt]
ex1 = 
      [
         Set "blah" (Val(Ival 33)),
         If (GT (Val (Ival 7)) (Val (Ival 5))) 
         (Set "Hollyfield" (Add (Val (Ival 1)) (Get "blah")))
         (Set "Hollyfield" (Add (Val (Ival 19)) (Val ((Ival 5)))))
      ]

--String Examples
ex2 :: [Stmt]
ex2 = 
      [
         Set "String1" (Val (Sval "Hello ")),
         Set "String2" (Val (Sval "World")),
         Set "String3" (Cat (Get "String1")(Get "String2")),
         Set "WordCount" (WC (Get "String3"))
      ]

--Error example
ex3 :: [Stmt]
ex3 = 
      [
         Set "hey" (Val(Ival 3)),
         Set "hey" (Val(Ival 4))
      ]

--Function example
ex4 :: [Stmt]
ex4 = 
      [
          Set "hello kitty" (Val (Ival 3)),
          Deffunc "func1" "param1" (Prog [Mutate "hello kitty" (Add (Get "param1") (Val (Ival 4)))]),
          Call "func1" (Ival 2),
          Call "func1" (Ival 7)
      ]
