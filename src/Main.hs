module Main where

import Text.Read (readMaybe)

data NumPrompt = NumPrompt
  { getPrompt :: !String
  , getMin :: !Int
  , getMax :: !Int
  }

gambleAmountPrompt :: NumPrompt
gambleAmountPrompt =
  NumPrompt "Please enter an amount to gamble between 1 and 10000: " 1 10000

betNumberPrompt :: NumPrompt
betNumberPrompt =
  NumPrompt "Please enter the number of bets between 1 and 8: " 1 8

playPrompt :: NumPrompt
playPrompt = NumPrompt prompt 1 2
  where
    prompt =
      "Please enter the number from the options below:\n\
          \1. Play again\n\
          \2. Walk away\n"

main :: IO ()
main = do
  putStrLn "Welcome to the game of Roulette!"
  initialBalance <- promptForNum gambleAmountPrompt
  balance <- playRoulette initialBalance
  putStrLn "Thank you for playing!"
  let finalNetGain = balance - initialBalance
   in if finalNetGain > 0
        then putStrLn "Congratulations!"
        else putStrLn "Better luck next time!"

playRoulette :: Int -> IO Int
playRoulette = playUntilGameOver
  where
    playUntilGameOver currentBalance = do
      numOfBets <- promptForNum betNumberPrompt
      putStrLn $ "current balance is: " ++ show currentBalance
      putStrLn $ "nubmer of bets are: " ++ show numOfBets
      putStrLn "pretending like user lost $100."
      if currentBalance > 0
        then do
          playAgain <- promptToPlayAgain
          if playAgain
            then playUntilGameOver (currentBalance - 100)
            else pure currentBalance
        else pure currentBalance

promptToPlayAgain :: IO Bool
promptToPlayAgain = (== 1) <$> promptForNum playPrompt

promptForNum :: NumPrompt -> IO Int
promptForNum prompt = prompUntilValid
  where
    min' = getMin prompt
    max' = getMax prompt
    errorMsg =
      concat
        [ "Not a valid integer in bounds of min: "
        , show min'
        , ", max: "
        , show max'
        ]
    prompUntilValid = do
      putStrLn $ getPrompt prompt
      line <- getLine
      case readMaybe line of
        Just n
          | n <= max' && n >= min' -> pure n
        _ -> do
          putStrLn errorMsg
          prompUntilValid
