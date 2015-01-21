module Main
(main) where

import Zendo.Functions
import Zendo.Evaluator
import Zendo.Parser
import Zendo.Language
import System.Random
import Data.Function

chooseFunc :: IO Func
chooseFunc = do
  i <- randomRIO (0, length funcs - 1)
  return $ funcs !! i

matches :: Func -> [(Int, Int, Int)]
matches func = [(a, b, c) | a <- [1..100], b <- [1..100], c <- [1..100], eval func a b c]

main :: IO ()
main = do
  putStrLn "Welcome to Zendo"
  putStrLn "I have generated a function for you to guess (use format 1,1,1)"
  func <- chooseFunc
  putStrLn $ "Here is a match: " ++ show (head (take 1 (matches func)))
  flip fix (0 :: Int) $ \loop score -> do
    putStrLn $ "Your Score: " ++ show score
    putStrLn "Guess:"
    guess <- getLine
    print (evalFromTuple func (read ("(" ++ guess ++ ")")))
    putStrLn "Guess the function? (Y/n)"
    yesNo <- getLine
    if yesNo == "Y"
      then do putStrLn "Function:"
              guessFunc <- getLine
              if parseString guessFunc == func
                then putStrLn "You win!!!"
                else do putStrLn "Wrong!"
                        loop (score + 10)
      else loop (score + 1)
