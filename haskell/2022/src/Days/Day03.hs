module Days.Day03 (runDay) where

import Data.List ( intersect )
import Data.Char (isUpper, ord)
import Data.Attoparsec.Text
    ( many1, sepBy, letter, endOfLine, Parser )
import Data.List.Split (chunksOf)

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 letter `sepBy` endOfLine

------------ TYPES ------------

type Rucksack = [Char]
type Input = [Rucksack]

type Output = Int

type Splitter = forall a . [a] -> [[a]]
type Joiner = forall a . (Eq a) => [[a]] -> [a]

------------ UTILS -------------

itemsInCommon :: Joiner
itemsInCommon = foldr1 intersect

score :: Char -> Int
score x = ord x - (if isUpper x then 38 else 96)

priority :: [[Char]] -> Int
priority = score . head . itemsInCommon -- Wish I didn't need this `head` - there's only ever 1 item in common!

------------ PART A ------------

halves :: Splitter
halves x = chunksOf midpoint x
  where
    midpoint = length x `div` 2

partA :: Input -> Output
partA = sum . map (priority . halves)

------------ PART B ------------

trios :: Splitter
trios = chunksOf 3

partB :: Input -> Output
partB = sum . map priority . trios
