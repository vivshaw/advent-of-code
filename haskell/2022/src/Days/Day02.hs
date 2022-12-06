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
  return UnresolvedGame {opponentMove, myMoveSymbol}

------------ TYPES ------------
data Move = Rock | Paper | Scissors deriving (Show)
data MyMoveSymbol = X | Y | Z deriving (Show)
data UnresolvedGame = UnresolvedGame
  { opponentMove :: Move
  , myMoveSymbol :: MyMoveSymbol
  } deriving (Show)
type Input = [UnresolvedGame]

type OutputA = Integer

type OutputB = Integer

------------ PART A ------------
data Outcome = Win | Tie | Lose
data ResolvedGame = ResolvedGame
  { myMove :: Move
  , outcome :: Outcome
  }
type Strategy = UnresolvedGame -> Move

play :: Strategy -> UnresolvedGame -> ResolvedGame
play strategy game =
  ResolvedGame {myMove, outcome}
  where
    myMove = strategy game
    outcome = case (opponentMove game, myMove) of
      (Rock, Scissors) -> Lose
      (Rock, Paper) -> Win
      (Scissors, Paper) -> Lose
      (Scissors, Rock) -> Win
      (Paper, Rock) -> Lose
      (Paper, Scissors) -> Win
      (_, _) -> Tie

score :: Strategy -> UnresolvedGame -> Integer
score strategy game = scoreMove + scoreOutcome
  where
    resolved = play strategy game
    scoreOutcome = case outcome resolved of
      Lose -> 0
      Tie -> 3
      Win -> 6
    scoreMove = case myMove resolved of
      Rock -> 1
      Paper -> 2
      Scissors -> 3

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
