module Libraries where

import Pear
import Prelude hiding (GT, LT)

-- for function runs given statement for given int amount of times
for :: Expr -> Stmt -> Stmt
for a b = Prog [Mutate "counter" (int 0), While (LT (Get "counter") (a)) (Prog [Inc "counter", b])]           



