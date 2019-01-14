{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (foldM)
import System.Random (randomRIO)
import Text.Read (readMaybe)

type Balance = Int

data NumPrompt = NumPrompt
  { getPrompt :: !String
  , getMin :: !Int
  , getMax :: !Int
  }

data Color
  = Red
  | Black
  deriving (Eq, Enum, Show)

data Parity
  = Even
  | Odd
  deriving (Eq, Enum, Show)

data Position
  = High
  | Low
  deriving (Eq, Enum, Show)

data DozenChoice
  = FirstDozen
  | SecondDozen
  | ThirdDozen
  deriving (Eq, Enum, Show)

data ColumnChoice
  = FirstColumn
  | SecondColumn
  | ThirdColumn
  deriving (Eq, Enum, Show)

data BetType
  = Single
  | RedBlack
  | EvenOdd
  | HighLow
  | Dozen
  | Column
  deriving (Enum, Show)

data BetChoice
  = SingleChoice Int
  | ColorChoice Color
  | ParityChoice Parity
  | PositionChoice Position
  | DozenChoice DozenChoice
  | ColumnChoice ColumnChoice
  deriving (Show)

data Bet = Bet
  { getAmount :: !Int
  , getChoice :: !BetChoice
  } deriving (Show)

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

playRoulette :: Balance -> IO Balance
playRoulette = playUntilGameOver
  where
    playUntilGameOver currentBalance = do
      bets <- collectBets currentBalance
      spin <- randomRIO (0, 36 :: Int)
      putStrLn $ "!!!!!!! Spun a " ++ show spin ++ " !!!!!!!"
      putStrLn $ "Your bets are: " ++ show bets
      putStrLn $ "current balance is: " ++ show currentBalance
      putStrLn "pretending like user lost $100."
      if currentBalance > 0
        then do
          playAgain <- promptToPlayAgain
          if playAgain
            then playUntilGameOver (currentBalance - 100)
            else pure currentBalance
        else pure currentBalance

collectBets :: Balance -> IO [Bet]
collectBets balance = do
  numOfBets <- promptForNum betNumberPrompt
  reverse <$> foldM promptForBet [] [1 .. numOfBets]
  where
    promptForBet :: [Bet] -> Int -> IO [Bet]
    promptForBet currentBets betNum = do
      putStrLn $ "\nBet number " ++ show betNum ++ ":"
      let betTotal = sum (map getAmount currentBets)
          available = balance - betTotal
       in do putStrLn $
               "The amount available to bet is: " ++ formatMoney available
             amount <- promptForNum betAmountPrompt
             if amount > available
               then let errorMsg =
                          concat
                            [ "You bet amount "
                            , formatMoney amount
                            , " exceeds the amount you have available to gamble "
                            , show available
                            , "\n"
                            , "Voiding current and any remaining bets.\n\n"
                            ]
                     in do putStrLn errorMsg
                           pure currentBets
               else do
                 betTypeNum <- promptForNum betTypePrompt
                 bet <- getBet amount $ toEnum (betTypeNum - 1)
                 pure $ bet : currentBets

getBet :: Int -> BetType -> IO Bet
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

formatMoney :: Int -> String
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

calcWinnings :: Int -> Bet -> Int
calcWinnings spin bet =
  let choice = getChoice bet
      amount = getAmount bet
      hitRed = isRedSquare spin
      isEven = spin `mod` 2 == 0
      hitColor c = spin /= 0 && (c == Red && hitRed || c == Black && not hitRed)
      hitParity p = spin /= 0 && (p == Even && isEven || p == Odd && not isEven)
      hitPosition p =
        p == High && spin >= 1 && spin <= 18 ||
        p == Low && spin >= 19 && spin <= 66
      hitDozen d =
        (d == FirstDozen && spin >= 1 && spin <= 12) ||
        (d == SecondDozen && spin >= 13 && spin <= 24) ||
        (d == ThirdDozen && spin >= 25 && spin <= 36)
      -- Observe that only numbers in column 3 is divisble by 3.
      -- Adding to the spin shifts from column 1 or 2 to 3.
      -- If we are then divisble by 3, then we know what column we came from.
      -- Therefore:
      -- Only a number n + 1 from column 2 is divisble by 3.
      -- Only a number n + 2 from column 1 is divisble by 3.
      hitColumn c =
        spin /= 0 &&
        (c == FirstColumn && (spin + 2) `mod` 3 == 0 ||
         (c == SecondColumn && (spin + 1) `mod` 3 == 0)) ||
        (c == ThirdColumn && spin `mod` 3 == 0)
   in case choice of
        SingleChoice n
          | n == spin -> amount * 35
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

-- | returns true if the given square is red. False implies the square is black
-- Assumes input is correctly between 1 and 36 inclusive.
isRedSquare :: Int -> Bool
isRedSquare n =
  let isEven = n `mod` 2 == 0
      isOdd = not isEven
   -- from observation the first 10 numbers red is odd, next 8 its even,
   -- next 10 its odd, and final 8 it's even again. Black also alternates
   -- for the same ranges of numbers.
   in (n >= 1 && n <= 10 && isOdd) ||
      (n >= 11 && n <= 18 && isEven) ||
      (n >= 19 && n <= 28 && isOdd) || (n >= 29 && n <= 26 && isEven)

gambleAmountPrompt :: NumPrompt
gambleAmountPrompt =
  NumPrompt "Please enter an amount to gamble between 1 and 10000: " 1 10000

betNumberPrompt :: NumPrompt
betNumberPrompt =
  NumPrompt "Please enter the number of bets between 1 and 8: " 1 8

betAmountPrompt :: NumPrompt
betAmountPrompt = NumPrompt prompt 1 500
  where
    prompt = "Please give a bet amount in whole numbers between 1 and 500: "

singleBetPrompt :: NumPrompt
singleBetPrompt =
  NumPrompt "Please select a number to bet on between 0 and 36: " 0 36

colorBetPrompt :: NumPrompt
colorBetPrompt = NumPrompt prompt 1 2
  where
    prompt =
      "Please select a color to bet on from the options below:\n\
        \1. Red\n\
        \2. Black\n"

parityBetPrompt :: NumPrompt
parityBetPrompt = NumPrompt prompt 1 2
  where
    prompt =
      "Please select to bet on even or odd from the options below:\n\
        \1. Even\n\
        \2. Odd\n"

positionBetPrompt :: NumPrompt
positionBetPrompt = NumPrompt prompt 1 2
  where
    prompt =
      "Please select to bet on high (1-18) or low (19-36) from the options below:\n\
        \1. High\n\
        \2. Low\n"

dozenBetPrompt :: NumPrompt
dozenBetPrompt = NumPrompt prompt 1 3
  where
    prompt =
      "Please select to which dozen to bet on from the options below:\n\
        \1. 1st Dozen (1-12)\n\
        \2. 2nd Dozen (13-24)\n\
        \3. 3rd Dozen (25-36)\n"

columnBetPrompt :: NumPrompt
columnBetPrompt = NumPrompt prompt 1 3
  where
    prompt =
      "Please select to which column to bet on from the options below:\n\
       \1. 1st column (1,4,7,10,13,16,19,22,25,28,31,34)\n\
       \2. 2nd column (2,5,8,11,14,17,20,23,26,29,32,35)\n\
       \3. 3rd column (3,6,9,12,15,18,21,24,27,30,33,36)\n"

betTypePrompt :: NumPrompt
betTypePrompt = NumPrompt prompt 1 6
  where
    prompt =
      "\n\
        \   Type         | payout | description\n\
        \----------------+--------+--------------------------------------\n\
        \1. Single number| 35:1   | pick a single number\n\
        \2. Red / Black  | 1:1    | pick red or black\n\
        \3. Even / Odd   | 1:1    | pick even or odd\n\
        \4. High / Low   | 1:1    | pick (1-18) or (19-36)\n\
        \5. Dozen        | 2:1    | pick (1-12), (13-24), or (25-36)\n\
        \6. Column       | 2:1    | pick an entire column\n\
        \\n\
        \Column 1: | 1 R | 4 B | 7 R | 10 B | 13 B | 16 R | 19 R | 22 B | 25 R | 28 B | 31 B | 34 R |\n\
        \Column 2: | 2 B | 5 R | 8 B | 11 B | 14 R | 17 B | 20 B | 23 R | 26 B | 29 B | 32 R | 35 B |\n\
        \Column 3: | 3 R | 6 B | 9 R | 12 R | 15 B | 18 R | 21 R | 24 B | 27 R | 30 R | 33 B | 36 R |\n\
        \\n\
        \Table key: R = Red, B = Black\n\n\
        \Please enter the number for the bet type from the table above: "

playPrompt :: NumPrompt
playPrompt = NumPrompt prompt 1 2
  where
    prompt =
      "Please enter the number from the options below:\n\
          \1. Play again\n\
          \2. Walk away\n"
