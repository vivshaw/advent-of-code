module Days.Day04 (runDay) where

import Data.Attoparsec.Text
    ( many1, sepBy, digit, char, endOfLine, Parser )

import qualified Program.RunDay as R (runDay, Day)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = pair `sepBy` endOfLine

pair :: Parser ElfPair
pair = do
  elf1 <- assignment
  _ <- char ','
  elf2 <- assignment
  return (elf1, elf2)

assignment :: Parser Assignment
assignment = do
  start <- read <$> many1 digit
  _ <- char '-'
  end <- read <$> many1 digit
  return (start, end)

------------ TYPES ------------
type Assignment = (Int, Int)
type ElfPair = (Assignment, Assignment)
type Input = [ElfPair]

type OutputA = Int

type OutputB = Int

------------ UTILS -------------
eitherWay :: ((t, t) -> Bool) -> (t, t) -> Bool
eitherWay fun (x, y) = fun (x, y) || fun (y, x)

countBy :: (a -> Bool) -> [a] -> Int
countBy predicate x = length $ filter predicate x
-- Why can't this be `countBy = length . filter`??

------------ PART A ------------
partA :: Input -> OutputA
partA = countBy $ eitherWay contains
  where
    contains (x, y) = fst x >= fst y && snd x <= snd y

------------ PART B ------------
partB :: Input -> OutputB
partB = countBy $ eitherWay overlaps
  where
    overlaps (x, y) = (fst x >= fst y && fst x <= snd y) || (snd x >= fst y && snd x <= snd y)
