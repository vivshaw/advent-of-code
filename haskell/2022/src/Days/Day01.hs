module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = elf `sepBy` endOfLine

elf :: Parser Elf
elf = food `sepBy` endOfLine

food :: Parser Food
food = read <$> many1 digit

------------ TYPES ------------
type Food = Integer
type Elf = [Food]
type Input = [Elf]

type Calories = Integer
type OutputA = Calories

type OutputB = Calories

------------ PART A ------------
partA :: Input -> OutputA
partA = maximum . calories
  where
    calories = map sum

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . top3 . ascending . calories
  where
    top3 = Data.List.take 3
    ascending = reverse . sort
    calories = map sum
    