-- | A single register imperative language.
module While where

import Prelude hiding (GT, LT)


--
-- * Syntax
--

--  Before refactoring:
--
--    int  ::= (any integer)
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

-- After refactoring to remove the possibility of type errors:

data Expr
   = Get
   | Lit Int
   | Add Expr Expr
  deriving (Eq,Show)

data Test
   = LTE Expr Expr 
   | LT  Expr Expr
   | GTE Expr Expr
   | GT  Expr Expr
   | EQU Expr Expr
   | NEQ Expr Expr
  deriving (Eq,Show)

data Stmt
   = Set Expr
   | While Test Stmt
   | If    Test Stmt Stmt
   | Begin [Stmt]
  deriving (Eq,Show)


-- Example program:
--   begin
--     R := 1
--     while R <= 100
--       R := R + R
--   end

--
-- * Semantics
--

type Reg = Int

-- Before refactoring:
--   expr: Reg -> Maybe (Either Int Bool)
--   stmt: Reg -> Maybe Reg
--
-- After refactoring:
--   expr: Reg -> Int
--   test: Reg -> Bool
--   stmt: Reg -> Reg

-- | Valuation function for expressions.
expr :: Expr -> Reg -> Int
expr Get       s = s
expr (Lit i)   s = i
expr (Add l r) s = expr l s + expr r s

-- | Valuation function for tests.
test :: Test -> Reg -> Bool
test (LTE l r) s = expr l s <= expr r s
test (LT l r) s  = expr l s <  expr r s
test (GTE l r) s = expr l s >= expr r s
test (GT l r) s  = expr l s >  expr r s
test (EQU l r) s = expr l s == expr r s
test (NEQ l r) s = expr l s /= expr r s

-- | Valuation function for statements.
stmt :: Stmt -> Reg -> Reg
stmt (Set e)     s = expr e s
stmt (While c b) s = if test c s then stmt (While c b) (stmt b s) else s
stmt (If c t e) s  = if test c s then stmt t s else stmt e s
stmt (Begin ss)  s = stmts ss s  -- foldl (flip stmt) s ss
  where
    stmts []     r = r
    stmts (s:ss) r = stmts ss (stmt s r)
