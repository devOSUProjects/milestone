module Progs where

import While

p :: Stmt
p = Begin
      [ Set (Lit 7),
        If  (EQU Get (Lit 8))
               (Set(Lit 9))
               (Set(Lit 11))
      ]
