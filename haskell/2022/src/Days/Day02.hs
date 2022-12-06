module Days.Day02 (runDay) where

{- ORMOLU_DISABLE -}

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Functor
import Control.Applicative
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseRound `sepBy` endOfLine

parseRound :: Parser UnresolvedGame
parseRound = do
  opponentMove <- char 'A' $> Rock <|> char 'B' $> Paper <|> char 'C' $> Scissors
  _ <- space
  myMoveSymbol <- char 'X' $> X <|> char 'Y' $> Y <|> char 'Z' $> Z
  return UnresolvedRound {opponentMove, myMoveSymbol}

------------ TYPES ------------
data Move = Rock | Paper | Scissors deriving (Show)
data MyMoveSymbol = X | Y | Z deriving (Show)
data UnresolvedGame = UnresolvedRound
  { opponentMove :: Move
  , myMoveSymbol :: MyMoveSymbol
  } deriving (Show)
type Input = [UnresolvedGame]

type OutputA = Integer

type OutputB = Integer

------------ PART A ------------
data Outcome = Win | Tie | Lose
data ResolvedGame = ResolvedRound
  { myMove :: Move
  , outcome :: Outcome
  }
type Strategy = UnresolvedGame -> Move

play :: Strategy -> UnresolvedGame -> ResolvedGame
play myStrategy game =
  ResolvedRound{myMove, outcome}
  where
    myMove = myStrategy game
    outcome = case (opponentMove game, myMove) of
      (Rock, Scissors) -> Lose
      (Rock, Paper) -> Win
      (Scissors, Paper) -> Lose
      (Scissors, Rock) -> Win
      (Paper, Rock) -> Lose
      (Paper, Scissors) -> Win
      (_, _) -> Tie

scoreOutcome :: ResolvedGame -> Integer
scoreOutcome game = case outcome game of
  Lose -> 0
  Tie -> 3
  Win -> 6

scoreMove :: ResolvedGame -> Integer
scoreMove game = case myMove game of
  Rock -> 1
  Paper -> 2
  Scissors -> 3

score :: Strategy -> UnresolvedGame -> Integer
score strategy game = scoreMove resolved + scoreOutcome resolved
  where
    resolved = play strategy game

naive :: Strategy
naive game = case myMoveSymbol game of
  X -> Rock
  Y -> Paper
  Z -> Scissors

partA :: Input -> OutputA
partA = sum . map points
  where
    points = score naive

------------ PART B ------------
correct :: Strategy
correct game = case (opponentMove game, myMoveSymbol game) of
  (Rock, X) -> Scissors
  (Rock, Y) -> Rock
  (Rock, Z) -> Paper
  (Paper, X) -> Rock
  (Paper, Y) -> Paper
  (Paper, Z) -> Scissors
  (Scissors, X) -> Paper
  (Scissors, Y) -> Scissors
  (Scissors, Z) -> Rock

partB :: Input -> OutputB
partB = sum . map points
  where
    points = score correct
