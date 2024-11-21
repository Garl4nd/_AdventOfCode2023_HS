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
    case problemId of
        Just x -> case solution x of
          Just resIO -> do 
	  	res <- resIO
		print $ "The solution of problem #" <> show x <> " is: " <> show res	
          Nothing ->print "Not yet solved"
        Nothing -> print "Not a number"                     --       return True
    mainLoop

solution :: Int -> Maybe (IO (Int, Int))
solution n =  case n of  
              1 -> Just $ getSolutions1 "inputs/1.txt"
              2 -> Just $ getSolutions2 "inputs/2.txt" 
              3 ->  Just $ getSolutions3 "inputs/3.txt" 
              4 -> Just $ getSolutions4 "inputs/4.txt" 
              5 -> Just $ getSolutions5 "inputs/5.txt" 
              6 -> Just $ getSolutions6 "inputs/6.txt" 
              7 -> Just $ getSolutions7 "inputs/7.txt" 
              8 -> Just $ getSolutions8 "inputs/8.txt" 
              9 -> Just $ getSolutions9 "inputs/9.txt"
              10 -> Just $ getSolutions10 "inputs/10.txt" 
              11 -> Just $ getSolutions11 "inputs/11.txt" 
              12 -> Just $ getSolutions12 "inputs/12.txt" 
              13 -> Just $ getSolutions13 "inputs/13.txt" 
              14 -> Just $ getSolutions14 "inputs/14.txt" 
              15 -> Just $ getSolutions15 "inputs/15.txt" 
              16 -> Just $ getSolutions16 "inputs/16.txt" 
              17 -> Just $ getSolutions17 "inputs/17.txt"              
              172 -> Just $ getSolutions17Old "inputs/17.txt"              
              173 -> Just $ getSolutions17Astar "inputs/17.txt"              
              18 -> Just $ getSolutions18 "inputs/18.txt"              
              19 -> Just $ getSolutions19 "inputs/19.txt"             
              20 -> Just $ getSolutions20 "inputs/20.txt"             
              21 -> Just $ getSolutions21 "inputs/21.txt"
              22 -> Just $ getSolutions22 "inputs/22.txt"
              23 -> Just $ getSolutions23 "inputs/23.txt"
              24 -> Just $ getSolutions24 "inputs/24.txt"
              25 -> Just $ getSolutions25 "inputs/25.txt"
              _ -> Nothing 

main :: IO ()
main = do
  mainLoop
  print "Goodbye!"
