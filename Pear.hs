module Pear where
import Prelude hiding (GT, LT)
import String


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
   | EQU Expr Expr
   | Cat Expr Expr
   | WC  Expr
   | Isset VarName
  deriving (Eq,Show)

data Stmt
   = Set VarName Expr
   | Mutate VarName Expr
   | While Expr Stmt
   | If    Expr Stmt Stmt
   | Deffunc  VarName ParamName Stmt  --
   | Call VarName Value
   | Prog [Stmt]
  deriving (Eq,Show)

data Value
   = Ival Int
   | Sval String
   | Bval Bool
   | Fval (ParamName, Stmt)
  deriving (Eq,Show)


--
-- * Semantics
--

type VarName = String
type ParamName = String
type Argument = [Value]
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
expr (Cat l r) s = case (expr l s, expr r s) of
                        (Just (Sval x), Just (Sval y)) -> Just (Sval (strcat x y))
                        _                              -> Nothing
expr (WC e) s = case expr e s of
                        Just (Sval x) -> Just (Ival (wordcount x))
                        _                              -> Nothing
expr (Isset ss) [] = Just (Bval False)
expr (Isset ss) ((n, i) : rr) = if n == ss then Just (Bval True) else expr (Isset ss) rr 

-- | Valuation function for statements.
stmt :: Stmt -> Vars -> Vars
stmt (Set r e) s   = case expr e s of
                     Just val -> if (expr (Isset r) s) == (Just (Bval False)) then (r, val) : s else error "Error: Value already set. You must mutate set variables"
                     Nothing  -> error "Error: Type error in code"
stmt (Mutate r e) s = case expr e s of
                      Just val -> map (\x -> if (fst x) == r then (r, val) else x) s
                      Nothing  -> error "Error: Type error in code"
stmt (If c t e) s  = case expr c s of
                     Just (Bval b) -> if b == True then stmt t s else stmt e s
                     _             -> error "Error: Type error in code"
stmt (While c t) s = case expr c s of
                     Just (Bval b) -> if b == True then stmt (While c t) (stmt t s) else s
                     _             -> error "Error: Type error in code"
stmt (Deffunc a b e) s = stmt (Prog [Set a (Val (Fval (b, e))), Set b (Val (Sval ""))]) s

stmt (Call a e)   s = case lookup a s of
                        Just (Fval (f, g)) -> stmt (Prog [Mutate f (Val e), g]) s
                        _                  -> error "Error: Type error in code"
stmt (Prog ss)  s = stmts ss s  -- foldl (flip stmt) s ss
  where
    stmts []     r = r
    stmts (s:ss) r = stmts ss (stmt s r)


-- Filters functions out of environmnet variable at end of program
filtervarlist :: Var -> Bool
filtervarlist (_, Fval _) = False 
filtervarlist _ = True

beginprog :: [Stmt] -> Vars
beginprog ss = filter (\x -> (filtervarlist x))  (stmt (Prog ss) [])



