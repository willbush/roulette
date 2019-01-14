module GameTypes where

type Balance = Int

type SquareNum = Int

type Amount = Int

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
  = SingleChoice SquareNum
  | ColorChoice Color
  | ParityChoice Parity
  | PositionChoice Position
  | DozenChoice DozenChoice
  | ColumnChoice ColumnChoice
  deriving (Show)

data Bet = Bet
  { getAmount :: !Amount
  , getChoice :: !BetChoice
  } deriving (Show)
