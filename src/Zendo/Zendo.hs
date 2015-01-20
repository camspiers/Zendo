module Zendo.Zendo where

import Zendo.Language
import Zendo.Evaluator
import Zendo.Parser
import Text.ParserCombinators.Parsec

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..]

variables :: [VarName]
variables = allValues

-- This is just a basic version
-- getFunctions :: [Function]
-- getFunctions = [
--   Function
--     (Expression (Variable var1) operator (Variable var2))
--     comparator
--     (Constant constant) |
--   var1       <- variables,
--   var2       <- variables,
--   comparator <- comparators,
--   operator   <- operators,
--   constant   <- [1..100],
--   var1 /= var2]

-- sources of Infinities: Quantity of Argument
--                        Quantity of Constant
--                        Quantity of Operation
--                        Domain of Constants
