module Wk5 where

data Type =
  Bool
  | Int
  | FunTy Type Type -- Type -> Type
    deriving (Show,Eq)

data Expr =
  Num Int
  | Lit Bool
  | If Expr Expr Expr
  | Apply Expr Expr
  | Plus Expr Expr
  | Eq Expr Expr
  | Recfun Type Type (Expr -> Expr -> Expr)
  | Var String -- only used for pretty-printing. Ignore.

-- TODO: Implement a Value type and an evaluator function for this language.

data Value = IntegerValue Int | BoolValue Bool | FuncValue (Expr -> Expr -> Expr)

uneval :: Value -> Expr
uneval (IntegerValue x) = Num x
uneval (BoolValue x) = Lit x
uneval (FuncValue x) = Recfun Bool Bool x -- just filled in the type with "Bool" but could be anything (we don't care anymore!)

evaluate :: Expr -> Value
evaluate (Num n) = IntegerValue n
evaluate (Lit b) = BoolValue b
evaluate (Recfun _ _ f) = FuncValue f
evaluate (Plus e1 e2) = 
    let (IntegerValue x1) = evaluate e1 -- since the program type-checked, we know e1 must evaluate to an IntegerValue
        (IntegerValue x2) = evaluate e2 in IntegerValue (x1 + x2)
evaluate (Eq e1 e2) = 
    let (IntegerValue x1) = evaluate e1
        (IntegerValue x2) = evaluate e2 in BoolValue (x1 == x2)
evaluate (If e1 e2 e3) =
    let (BoolValue b) = evaluate e1 in 
        evaluate (if b then e2 else e3)
evaluate (Apply e1 e2) =
    let (FuncValue f) = evaluate e1 in
        let x = evaluate e2 in
            evaluate (f (Recfun Bool Bool f) (uneval x)) -- just filled in the type with "Bool" but could be anything (we don't care anymore!)
evaluate (Var _) = undefined -- explicity ignore handling this case
