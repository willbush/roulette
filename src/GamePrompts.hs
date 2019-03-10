{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module GamePrompts where

import           GameTypes
import           RIO
import           Util      (showText)

gambleAmountPrompt :: NumPrompt
gambleAmountPrompt =
  NumPrompt "Please enter an amount to gamble between 1 and 10000: " 1 10000

betNumberPrompt :: NumPrompt
betNumberPrompt = NumPrompt prompt 1 8
  where
    prompt =
      "Please enter the number of bets between 1 and 8:\n\
        \Note that each bet may not exceed $500:"

betAmountPrompt :: Balance -> NumPrompt
betAmountPrompt balanceAvailable = NumPrompt prompt 1 maxBetAmount
  where
    maxBetAmount = min 500 balanceAvailable
    prompt =
      "Please give a bet amount in whole numbers between 1 and "
        <> showText maxBetAmount
        <> ": "

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
