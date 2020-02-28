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
   = Get VarName
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
   | Isset VarName
  deriving (Eq,Show)

data Stmt
   = Set VarName Expr
   | Mutate VarName Expr
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

type VarName = String
type Var = (VarName, Int)
type Vars = [Var]

-- Before refactoring:
--   expr: Reg -> Maybe (Either Int Bool)
--   stmt: Reg -> Maybe Reg
--
-- After refactoring:
--   expr: Reg -> Int
--   test: Reg -> Bool
--   stmt: Reg -> Reg

-- | Valuation function for expressions.
expr :: Expr -> Vars -> Int
expr (Get s) [] = 0
expr (Get s) (v : vv) = case v of
                        (n, i) -> if s == n then i else expr (Get s) vv
expr (Lit i)   s = i
expr (Add l r) s = expr l s + expr r s

-- | Valuation function for tests.
test :: Test -> Vars -> Bool
test (LTE l r) s = expr l s <= expr r s
test (LT l r) s  = expr l s <  expr r s
test (GTE l r) s = expr l s >= expr r s
test (GT l r) s  = expr l s >  expr r s
test (EQU l r) s = expr l s == expr r s
test (NEQ l r) s = expr l s /= expr r s
test (Isset ss) [] = False
test (Isset ss) (r : rr) = case r of
                             (n, i) -> if n == ss then True else test (Isset ss) rr 

-- | Valuation function for statements.
stmt :: Stmt -> Vars -> Vars
stmt (Set r e) s   = if (test (Isset r) s) == False then (r, (expr e s)) : s else s 
stmt (Mutate r e) s = map (\x -> if (fst x) == r then (r, (expr e s)) else x) s
stmt (While c b) s = if test c s then stmt (While c b) (stmt b s) else s
stmt (If c t e) s  = if test c s then stmt t s else stmt e s
stmt (Begin ss)  s = stmts ss s  -- foldl (flip stmt) s ss
  where
    stmts []     r = r
    stmts (s:ss) r = stmts ss (stmt s r)

--compares two string return true if they are equal, else false
strcmp :: String -> String -> Bool
strcmp [] [] = True
strcmp a [] = False
strcmp [] a = False
strcmp (x:xs) (s:ss) = if ( x == s ) then strcmp xs ss else False

--when given two strings it will concatenate them
strcat :: [[a]] -> [[a]] -> [[a]]
strcat [] _ = []
strcat _ [] = []
strcat (x:xs) (y:ys) = (x ++ y) : strcat xs ys

--Returns the number of words in a word list (String)
wordcount :: String -> Int
wordcount = length . words

--Checks if a given string is contained within the second string.
--The string must be contained exactly as typed in the 2nd string or it will return false.
strhas :: String -> String -> Bool
strhas (_:_) [] = False
strhas xs ys
   | prefix xs ys = True
   | strhas xs (tail ys) = True
   | otherwise = False
   
prefix :: String -> String -> Bool
prefix [] _ = True
prefix (_:_) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

