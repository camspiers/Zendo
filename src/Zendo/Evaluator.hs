module Zendo.Evaluator where

import Zendo.Language

evalExpr :: Expr -> Int -> Int -> Int -> Int
evalExpr (Var A)        a _ _ = a
evalExpr (Var B)        _ b _ = b
evalExpr (Var C)        _ _ c = c
evalExpr (Val value)    _ _ _ = value
evalExpr (Plus l r)     a b c = (+) (evalExpr l a b c) (evalExpr r a b c)
evalExpr (Multiply l r) a b c = (*) (evalExpr l a b c) (evalExpr r a b c)
evalExpr (Minus l r)    a b c = (-) (evalExpr l a b c) (evalExpr r a b c)

eval :: Func -> Int -> Int -> Int -> Bool
eval (GreaterThan l r) a b c = (>)  (evalExpr l a b c) (evalExpr r a b c)
eval (LessThan    l r) a b c = (<)  (evalExpr l a b c) (evalExpr r a b c)
eval (Equal       l r) a b c = (==) (evalExpr l a b c) (evalExpr r a b c)
eval (And         l r) a b c = (&&) (eval l a b c) (eval r a b c)
eval (Or          l r) a b c = (||) (eval l a b c) (eval r a b c)
