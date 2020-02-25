module Progs where

import While


--It's returning a list of variables now
--Should return Var1 == 7, Var2 == 33
p :: Stmt
p = Begin
      [ Set "Var1" (Lit 7),
        If  (EQU (Get "Var1") (Lit 8))
               (Set "Var2" (Lit 9))
               (Set "Var2" (Lit 11)),
        If (EQU (Get "Var2") (Lit 11))
                (Mutate "Var2" (Lit 33))
                (Mutate "Var2" (Lit 99))
      ] 
