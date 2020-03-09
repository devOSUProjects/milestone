module Progs where

import Pear
import Prelude hiding (GT, LT)


ex1 :: Stmt
ex1 = Prog
      [
         Set "blah" (Val(Ival 33)),
         If (GT (Val (Ival 7)) (Val (Ival 5))) 
         (Set "Hollyfield" (Add (Val (Ival 1)) (Get "blah")))
         (Set "Hollyfield" (Add (Val (Ival 19)) (Val ((Ival 5)))))
      ]

ex2 :: Stmt
ex2 = Prog
      [
         Set "String1" (Val (Sval "Hello ")),
         Set "String2" (Val (Sval "Doc")),
         Set "String3" (Cat (Get "String1")(Get "String2")),
         Set "WordCount" (WC (Get "String3"))
      ]

