module Main
  ( main
  ) where

import Control.Monad (unless)
import Text.Read (readMaybe)
import Lib

mainLoop :: IO ()
mainLoop = do
  print "Which problem do you want to solve?"
  prompt <- getLine
  unless (prompt `elem` ["e", "end"]) $ do
    let problemId = readMaybe prompt
    do
      case problemId of
        Just x
          | x `elem` [1, 2, 3, 4, 5, 6, 7, 8, 9, 24] -> do
            print $ "The solution of problem #" <> show x <> " is:"
            case x of
              1 -> getSolutions1 "inputs/1.txt" >>= print
              2 -> getSolutions2 "inputs/2.txt" >>= print
              3 -> getSolutions3 "inputs/3.txt" >>= print
              4 -> getSolutions4 "inputs/4.txt" >>= print
              5 -> getSolutions5 "inputs/5.txt" >>= print
              6 -> getSolutions6 "inputs/6.txt" >>= print
              7 -> getSolutions7 "inputs/7.txt" >>= print
              8 -> getSolutions8 "inputs/8.txt" >>= print
              9 -> getSolutions9 "inputs/9.txt" >>= print
              24 -> getSolutions24 "inputs/24.txt" >>= print
              _ -> print "Program error"
        Just _ -> print "Not yet solved"
        Nothing -> print "Not a number"
                                --       return True
    mainLoop

main :: IO ()
main = do
  mainLoop
  print "Goodbye!"
