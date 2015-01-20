module Zendo.Parser where

import Zendo.Language
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

skipSpaces :: Parser a -> Parser a 
skipSpaces parser = do r <- parser
                       _ <- spaces
                       return r

infixBinOp s f = Infix (do skipSpaces (string s); return f)

opTable = [[infixBinOp "*" Multiply AssocLeft],
           [infixBinOp "+" Plus AssocLeft, infixBinOp "-" Minus AssocLeft]]

booleanOpTable = [[infixBinOp "&&" And AssocLeft, infixBinOp "||" Or AssocLeft]]

exprTerm :: Parser Expr
exprTerm = skipSpaces exprValue <|> exprVariable <?> "expression term"

exprValue :: Parser Expr
exprValue = do v <- many1 digit; return (Val (read v))
            <?> "expression value"

exprVariable :: Parser Expr
exprVariable = var 'a' A <|> var 'b' B <|> var 'c' C
               <?> "expression variable"
               where var c v = do _ <- skipSpaces (char c); return (Var v)

expr :: Parser Expr
expr = buildExpressionParser opTable exprTerm

funcTerm :: Parser Func
funcTerm = do l <- expr
              c <- com ">" GreaterThan <|> com "<" LessThan <|> com "=" Equal
              r <- expr
              return (c l r)
           <?> "function term"
           where com s f = do skipSpaces (string s); return f

func :: Parser Func
func = buildExpressionParser booleanOpTable funcTerm

parseString :: String -> Func
parseString str =
  case parse func "" str of
    Left e  -> error $ show e
    Right r -> r
