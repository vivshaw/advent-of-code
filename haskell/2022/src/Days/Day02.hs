module Days.Day02 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseRound `sepBy` endOfLine

parseRound :: Parser Round
parseRound = do
  opponentMove <- toOppMove <$> satisfy (inClass "ABC")
  space
  myMove <- toMyMove <$> satisfy (inClass "XYZ")
  return Round {opponentMove, myMove}

toOppMove :: Char -> OppMoveSymbol
toOppMove 'A' = A
toOppMove 'B' = B
toOppMove 'C' = C

toMyMove :: Char -> MyMoveSymbol
toMyMove 'X' = X
toMyMove 'Y' = Y
toMyMove 'Z' = Z

------------ TYPES ------------
data OppMoveSymbol = A | B | C deriving (Show)
data MyMoveSymbol = X | Y | Z deriving (Show)
data Round = Round
  { opponentMove :: OppMoveSymbol
  , myMove :: MyMoveSymbol
  } deriving (Show)
type Input = [Round]

type OutputA = Integer

type OutputB = Integer

------------ PART A ------------
data Move = Rock | Paper | Scissors
data Outcome = Win | Tie | Lose

play :: Move -> Move -> Outcome
play Rock Scissors = Lose
play Rock Paper = Win
play Scissors Paper = Lose
play Scissors Rock = Win
play Paper Rock = Lose
play Paper Scissors = Win
play _ _ = Tie

oppMove :: Round -> Move
oppMove Round{opponentMove=A} = Rock
oppMove Round{opponentMove=B} = Paper
oppMove Round{opponentMove=C} = Scissors

myNaiveMove :: Round -> Move
myNaiveMove Round{myMove=X} = Rock
myNaiveMove Round{myMove=Y} = Paper
myNaiveMove Round{myMove=Z} = Scissors
        
scoreNaiveGame :: Round -> Integer
scoreNaiveGame game = scoreOutcome $ play (oppMove game) (myNaiveMove game)  

scoreOutcome :: Outcome -> Integer
scoreOutcome outcome = case outcome of
  Lose -> 0
  Tie -> 3
  Win -> 6

scoreMove :: Move -> Integer
scoreMove move = case move of
  Rock -> 1
  Paper -> 2
  Scissors -> 3

scoreNaive :: Round -> Integer
scoreNaive round = scoreMove (myNaiveMove round) + scoreNaiveGame round

partA :: Input -> OutputA
partA = sum . map scoreNaive

------------ PART B ------------
toCorrectMove :: Round -> Move
toCorrectMove Round{opponentMove=A, myMove=X} = Scissors
toCorrectMove Round{opponentMove=A, myMove=Y} = Rock
toCorrectMove Round{opponentMove=A, myMove=Z} = Paper
toCorrectMove Round{opponentMove=B, myMove=X} = Rock
toCorrectMove Round{opponentMove=B, myMove=Y} = Paper
toCorrectMove Round{opponentMove=B, myMove=Z} = Scissors
toCorrectMove Round{opponentMove=C, myMove=X} = Paper
toCorrectMove Round{opponentMove=C, myMove=Y} = Scissors
toCorrectMove Round{opponentMove=C, myMove=Z} = Rock

scoreCorrectGame :: Round -> Integer
scoreCorrectGame game = scoreOutcome $ play (oppMove game) (toCorrectMove game)  

scoreCorrect :: Round -> Integer
scoreCorrect round = scoreMove (toCorrectMove round) + scoreCorrectGame round

partB :: Input -> OutputB
partB = sum . map scoreCorrect
