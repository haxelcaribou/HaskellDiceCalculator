{-# LANGUAGE TupleSections #-}
module Dice (rollDice) where

import Data.Bifunctor (first, second)
import Data.List (sort)
import System.Random

-- return list of n number of s sided dice and new StdGen
-- return Nothing if rolling negative number of dice or with negative number of sides
getDiceRoll :: StdGen -> Int -> Int -> Maybe ([Int], StdGen)
getDiceRoll g n s
  | n < 0 = Nothing
  | s <= 0 = Nothing
  | n == 0 = Just ([], g)
  | otherwise =
      let r = randomR (1, s) g
       in first (fst r :) <$> getDiceRoll (snd r) (n - 1) s

-- roll n number of s sided dice
rollDice :: StdGen -> Int -> Int -> Maybe (Int, StdGen)
rollDice g n s = first sum <$> getDiceRoll g n s

-- roll n number of s sided dice dice, removing either highest or lowest r elements
rollAndRemoveDice :: StdGen -> Int -> Int -> Int -> Bool -> Maybe (Int, StdGen)
rollAndRemoveDice g n s r t = first sum <$> (getDiceRoll g n s >>= (\d -> (, snd d) <$> removeDice (fst d) r t))

-- remove highest n elements from list if t is true
-- otherwise remove lowest n elements
-- return Nothing if n is negative or attempting to remove more elements than exist
removeDice :: [Int] -> Int -> Bool -> Maybe [Int]
removeDice [] _ _ = Just []
removeDice l n t
  | n < 0 = Nothing
  | n == 0 = Just l
  | t = reverse <$> removeSortedDice (reverse (sort l)) n
  | otherwise = removeSortedDice (sort l) n

-- drop n elements from beginning of list
-- return Nothing if n is negative or attempting to remove more elements than exist
removeSortedDice :: [Int] -> Int -> Maybe [Int]
removeSortedDice l 0 = Just l
removeSortedDice [] _ = Nothing
removeSortedDice l n
  | n < 0 = Nothing
  | otherwise = Just $ drop n l