module Zendo.Language where

data VarName = A
             | B
             | C deriving (Eq, Enum, Bounded)

data Expr = Var VarName
          | Val Int
          | Plus Expr Expr
          | Multiply Expr Expr
          | Minus Expr Expr deriving (Eq)

data Func = GreaterThan Expr Expr
          | LessThan Expr Expr
          | Equal Expr Expr
          | And Func Func
          | Or Func Func deriving (Eq)

instance Show VarName where
  show (A) = "a"
  show (B) = "b"
  show (C) = "c"

instance Show Expr where
  show (Var variable)        = show variable
  show (Val value)           = show value
  show (Plus left right)     = "(" ++ show left ++ " + " ++ show right ++ ")"
  show (Multiply left right) = "(" ++ show left ++ " * " ++ show right ++ ")"
  show (Minus left right)    = "(" ++ show left ++ " - " ++ show right ++ ")"

instance Show Func where
  show (GreaterThan left right) = showExpr left ++ " > " ++ showExpr right
  show (LessThan left right)    = showExpr left ++ " < " ++ showExpr right
  show (Equal left right)       = showExpr left ++ " = " ++ showExpr right
  show (And left right)         = show left ++ "\nand\n" ++ show right
  show (Or left right)          = show left ++ "\nor\n" ++ show right

showExpr :: Expr -> String
showExpr (Plus left right)     = show left ++ " + " ++ show right
showExpr (Multiply left right) = show left ++ " * " ++ show right
showExpr (Minus left right)    = show left ++ " - " ++ show right
showExpr expr = show expr
