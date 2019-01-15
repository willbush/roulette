module Main where

import Data.List     (foldl')
import GamePrompts
import GameTypes
import System.Random (randomRIO)
import Text.Read     (readMaybe)

main :: IO ()
main = do
  putStrLn "Welcome to the game of Roulette!"
  initialBalance <- promptForNum gambleAmountPrompt
  finalBalance <- playRoulette initialBalance
  putStrLn "Thank you for playing!"
  if finalBalance - initialBalance > 0
    then putStrLn "Congratulations!"
    else putStrLn "Better luck next time!"

playRoulette :: Balance -> IO Balance
playRoulette balance = do
  bets <- collectBets balance
  spin <- randomRIO (0, 36 :: SquareNum)
  putStrLn $ "!!!!!!! Spun a " ++ show spin ++ " !!!!!!!"
  let newBalance = foldl' calcNewBalance balance bets
      calcNewBalance :: Balance -> Bet -> Balance
      calcNewBalance currentBalance bet =
        let winnings = calcWinnings spin bet
         in if winnings > 0
              then currentBalance + winnings
              else currentBalance - getAmount bet
   in do putStrLn $ "Net Gain: " ++ formatMoney (newBalance - balance)
         putStrLn $ "Your Balance: " ++ formatMoney newBalance
         if newBalance > 0
           then do
             playAgain <- promptToPlayAgain
             if playAgain
               then playRoulette newBalance
               else pure newBalance
           else pure newBalance

collectBets :: Balance -> IO [Bet]
collectBets balance = do
  numOfBets <- promptForNum betNumberPrompt
  go [1 .. numOfBets] balance []
  where
    go [] _ bets = pure bets
    go _ 0 bets = pure bets
    go (betNum:betNums) available bets = do
      putStrLn $ "\nBet number " ++ show betNum ++ ":"
      putStrLn $ "The amount available to gamble is: " ++ formatMoney available
      amount <- promptForNum $ betAmountPrompt available
      betTypeNum <- promptForNum betTypePrompt
      bet <- getBet amount $ toEnum (betTypeNum - 1)
      go betNums (available - getAmount bet) (bet : bets)

getBet :: Amount -> BetType -> IO Bet
getBet amount t =
  case t of
    Single -> do
      n <- promptForNum singleBetPrompt
      pure $ Bet amount $ SingleChoice n
    RedBlack -> do
      n <- promptForNum colorBetPrompt
      pure $ Bet amount $ ColorChoice $ toEnum (n - 1)
    EvenOdd -> do
      n <- promptForNum parityBetPrompt
      pure $ Bet amount $ ParityChoice $ toEnum (n - 1)
    HighLow -> do
      n <- promptForNum positionBetPrompt
      pure $ Bet amount $ PositionChoice $ toEnum (n - 1)
    Dozen -> do
      n <- promptForNum dozenBetPrompt
      pure $ Bet amount $ DozenChoice $ toEnum (n - 1)
    Column -> do
      n <- promptForNum columnBetPrompt
      pure $ Bet amount $ ColumnChoice $ toEnum (n - 1)

formatMoney :: Amount -> String
formatMoney x =
  if x < 0
    then "-$" ++ show x
    else "$" ++ show x

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

-- | Calculates the winnings based on the bet and the number that got spun.
calcWinnings :: SquareNum -> Bet -> Int
calcWinnings n bet =
  let choice = getChoice bet
      amount = getAmount bet
      isEven = n `mod` 2 == 0
      isOdd = not isEven
      -- from observation the first 10 numbers red is odd, next 8 its even,
      -- next 10 its odd, and final 8 it's even again. Black also alternates
      -- for the same ranges of numbers.
      -- Note that 0 is neither red or black
      hitRed =
        n /= 0 &&
        ((n >= 1 && n <= 10 && isOdd) ||
         (n >= 11 && n <= 18 && isEven) ||
         (n >= 19 && n <= 28 && isOdd) || (n >= 29 && n <= 26 && isEven))
      hitBlack = n /= 0 && not hitRed
      hitColor c = c == Red && hitRed || c == Black && hitBlack
      -- Note that 0 is neither even or odd in Roulette
      hitParity p = n /= 0 && (p == Even && isEven || p == Odd && not isEven)
      hitPosition p =
        p == High && n >= 1 && n <= 18 || p == Low && n >= 19 && n <= 66
      hitDozen d =
        (d == FirstDozen && n >= 1 && n <= 12) ||
        (d == SecondDozen && n >= 13 && n <= 24) ||
        (d == ThirdDozen && n >= 25 && n <= 36)
      -- Observe that only numbers in column 3 is divisble by 3.
      -- Adding to the square number shifts from column 1 or 2 to 3.
      -- If we are then divisble by 3, then we know what column we came from.
      -- Therefore:
      -- Only a number n + 1 from column 2 is divisble by 3.
      -- Only a number n + 2 from column 1 is divisble by 3.
      -- Note that 0 is not on a column.
      hitColumn c =
        n /= 0 &&
        (c == FirstColumn && (n + 2) `mod` 3 == 0 ||
         (c == SecondColumn && (n + 1) `mod` 3 == 0)) ||
        (c == ThirdColumn && n `mod` 3 == 0)
   in case choice of
        SingleChoice c
          | c == n -> amount * 35
        ColorChoice color
          | hitColor color -> amount
        ParityChoice parity
          | hitParity parity -> amount
        PositionChoice pos
          | hitPosition pos -> amount
        DozenChoice dozen
          | hitDozen dozen -> amount * 2
        ColumnChoice col
          | hitColumn col -> amount * 2
        _ -> 0
