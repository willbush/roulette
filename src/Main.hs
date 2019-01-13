module Main where

import Control.Monad (unless)
import Text.Read (readMaybe)

gambleAmountPrompt :: String
gambleAmountPrompt = "Please enter an amount to gamble between 1 and 10000: "

main :: IO ()
main = do
  num <- promptForNum gambleAmountPrompt 1 10000
  putStrLn $ "You entered " ++ show num
  putStrLn "enter q to quit."
  line <- getLine
  unless (line == "q") main

promptForNum :: String -> Int -> Int -> IO Int
promptForNum prompt min' max' = prompUntilValid
  where
    errorMsg =
      concat
        [ "Not a valid integer in bounds of min: "
        , show min'
        , ", max: "
        , show max'
        ]
    prompUntilValid = do
      putStrLn prompt
      line <- getLine
      case readMaybe line of
        Just n
          | n <= max' && n >= min' -> pure n
        _ -> do
          putStrLn errorMsg
          prompUntilValid
