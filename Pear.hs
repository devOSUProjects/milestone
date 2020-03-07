module Pear where
import Prelude hiding (GT, LT)


--
-- * Syntax
--
--    Int ::= (any integer)
--    String ::= (any String)
--    Bool ::= True | False
--
--    expr ::= `R`                  -- load from register
--          |  int                  -- integer literal
--          |  expr `+` expr        -- addition expression
--          |  expr `≤` expr        -- less than or equal to
--
--    stmt ::= `R :=` expr          -- set register
--          |  `while` expr stmt    -- while loop
--          |  `begin` stmt* `end`  -- statement block
--

data Expr
   = Get VarName
   | Val Value
   | Add Expr Expr
   | LT  Expr Expr
   | GT  Expr Expr
   | EQU  Expr Expr
   | Isset VarName
  deriving (Eq,Show)

data Stmt
   = Set VarName Expr
   | Mutate VarName Expr
   | While Expr Stmt
   | If    Expr Stmt Stmt
   | Prog [Stmt]
  deriving (Eq,Show)

data Value
   = Ival Int
   | Sval String
   | Bval Bool
  deriving (Eq,Show)


--
-- * Semantics
--

type VarName = String
type Var = (VarName, Value)
type Vars = [Var]

-- | Valuation function for expressions.
expr :: Expr -> Vars -> Maybe Value
expr (Val i)   s = Just i
expr (Get s) [] = Nothing
expr (Get s) ((n, i) : vv) = if s == n then Just i else expr (Get s) vv
expr (Add l r) s = case (expr l s, expr r s) of
                        (Just (Ival x), Just (Ival y)) -> Just (Ival (x + y))
                        _                              -> Nothing
expr (GT l r) s = case (expr l s, expr r s) of
                        (Just (Ival x), Just (Ival y)) -> if x > y then Just (Bval True) else Just (Bval False)
                        _                              -> Nothing
expr (LT l r) s = case (expr l s, expr r s) of
                        (Just (Ival x), Just (Ival y)) -> if x < y then Just (Bval True) else Just (Bval False)
                        _                              -> Nothing
expr (EQU l r) s = case (expr l s, expr r s) of
                        (Just (Ival x), Just (Ival y)) -> if x == y then Just (Bval True) else Just (Bval False)
                        _                              -> Nothing
expr (Isset ss) [] = Just (Bval False)
expr (Isset ss) ((n, i) : rr) = if n == ss then Just (Bval True) else expr (Isset ss) rr 

-- | Valuation function for statements.
stmt :: Stmt -> Vars -> Vars
stmt (Set r e) s   = case expr e s of
                     Just val -> if (expr (Isset r) s) == (Just (Bval False)) then (r, val) : s else s
                     Nothing  -> s
stmt (Mutate r e) s = case expr e s of
                      Just val -> map (\x -> if (fst x) == r then (r, val) else x) s
                      Nothing  -> s
stmt (If c t e) s  = case expr c s of
                     Just (Bval b) -> if b == True then stmt t s else stmt e s
                     _             -> s
stmt (While c t) s = case expr c s of
                     Just (Bval b) -> if b == True then stmt (While c t) (stmt t s) else s
                     _             -> s
stmt (Prog ss)  s = stmts ss s  -- foldl (flip stmt) s ss
  where
    stmts []     r = r
    stmts (s:ss) r = stmts ss (stmt s r)

