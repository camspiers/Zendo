module Zendo.Evaluator
( eval
, Args ) where

import Zendo.Language

type Args = (Int, Int, Int)

evalExpr :: Expr -> Args -> Int
evalExpr (Var A)        (a, _, _) = a
evalExpr (Var B)        (_, b, _) = b
evalExpr (Var C)        (_, _, c) = c
evalExpr (Val value)    (_, _, _) = value
evalExpr (Plus l r)     args = (+) (evalExpr l args) (evalExpr r args)
evalExpr (Multiply l r) args = (*) (evalExpr l args) (evalExpr r args)
evalExpr (Minus l r)    args = (-) (evalExpr l args) (evalExpr r args)

eval :: Func -> Args -> Bool
eval (GreaterThan l r) args = (>)  (evalExpr l args) (evalExpr r args)
eval (LessThan    l r) args = (<)  (evalExpr l args) (evalExpr r args)
eval (Equal       l r) args = (==) (evalExpr l args) (evalExpr r args)
eval (And         l r) args = (&&) (eval l args) (eval r args)
eval (Or          l r) args = (||) (eval l args) (eval r args)
