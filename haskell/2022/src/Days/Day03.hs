module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Char (isUpper, ord)
import Data.Attoparsec.Text
import Data.List.Split (chunksOf)

import qualified Program.RunDay as R (runDay, Day)
import Data.Foldable (Foldable(toList))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 letter `sepBy` endOfLine

------------ TYPES ------------
type Rucksack = [Char]
type Input = [Rucksack]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
intersection :: (Eq a) => [[a]] -> [a]
intersection = foldr1 intersect

decode :: Char -> Int
decode x = ord x - (if isUpper x then 38 else 96)

halve :: forall a . [a] -> [[a]]
halve x = toList (splitAt half x)
   where
    half = length x `div` 2

partA :: Input -> OutputA
partA = sum . map priority
  where
    priority = decode . head . intersection . halve

------------ PART B ------------

trios :: forall a . [a] -> [[a]]
trios = chunksOf 3

partB :: Input -> OutputB
partB = sum . priorities
  where
    priorities = map (decode . head . intersection) . trios
