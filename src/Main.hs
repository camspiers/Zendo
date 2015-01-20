import Zendo.Zendo

-- import Data.Function
-- import System.Random

main :: IO ()
main = do
  putStrLn "Welcome to Zendo"
--   putStrLn "I have generated a function for you to guess"
--   secretFunc <- getRandomTuple
--   putStrLn $ showFunc secretFunc
--   flip fix (0 :: Int) $ \loop a -> do
--     putStr "Guess: "
--     guess <- getLine
--     let
--       zendo = functionFromTuple secretFunc (read guess)
--     print zendo
--     loop (a+1)
