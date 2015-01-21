module Zendo.Functions
( funcs ) where

import Zendo.Language

vars :: [Expr]
vars = [Var A, Var B, Var C]

vals :: [Expr]
vals = [Val v | v <- [1..10]]

ops :: [Expr -> Expr -> Expr]
ops = [Plus, Minus, Multiply]

coms :: [Expr -> Expr -> Func]
coms = [GreaterThan, LessThan, Equal]

rExprs :: [Expr]
rExprs = vars ++ vals

lExprs :: [Expr]
lExprs = [op l r |
  op <- ops,
  l  <- rExprs,
  r  <- rExprs,
  not (isVal l && isVal r),
  not (isMultiply (op l r) && isVal r && r == Val 1),
  l /= r]

funcs :: [Func]
funcs = [com l r |
  com <- coms,
  l <- rExprs ++ lExprs,
  r <- rExprs,
  not (isVal l && isVal r),
  not (isVal l && isVar r),
  l /= r]
