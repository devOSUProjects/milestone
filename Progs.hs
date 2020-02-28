module Progs where

import While
import Prelude hiding (GT, LT)


--It's returning a list of variables now
--Should return Var1 == 7, Var2 == 33

--good
ex1 :: Stmt
ex1 = Begin
      [ Set "Var1" (Lit 7),
        If  (EQU (Get "Var1") (Lit 8))
               (Set "Var2" (Lit 9))
               (Set "Var2" (Lit 11)),
        If (EQU (Get "Var2") (Lit 11))
                (Mutate "Var2" (Lit 33))
                (Mutate "Var2" (Lit 99))
      ]
--good
ex2 :: Stmt
ex2 = Begin
      [ Set "Var1" (Lit 0),
        Set "Var2" (Lit 0),
        While (LT (Get "Var1") (Lit 10))
                (Begin
                [Mutate "Var1" (Add (Get "Var1") (Lit 1)),
                 Mutate "Var2" (Add (Get "Var2") (Lit 50))
                ])
      ]

--bad
ex3 :: Stmt
ex3 = Begin
      [ Set "Var2" (Lit 3), 
        Mutate "Var2" (Get "Var1")
      ]

