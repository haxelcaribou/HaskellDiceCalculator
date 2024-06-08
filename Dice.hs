{-# LANGUAGE TupleSections #-}
module Dice (rollDice) where

import Error
import Data.Bifunctor (first, second)
import Data.List (sort)
import System.Random

-- return list of n number of s sided dice and new StdGen
-- return Nothing if rolling negative number of dice or with negative number of sides
getDiceRoll :: StdGen -> Int -> Int -> ErrorProne ([Int], StdGen)
getDiceRoll g n s
  | n < 0 = Left "cannot roll a negative number of dice"
  | s < 0 = Left "dice must have a positive number of sides"
  | n == 0 || s == 0 = Right ([], g)
  | otherwise =
      let r = randomR (1, s) g
       in first (fst r :) <$> getDiceRoll (snd r) (n - 1) s

-- roll n number of s sided dice
rollDice :: StdGen -> Int -> Int -> ErrorProne (Int, StdGen)
rollDice g n s = first sum <$> getDiceRoll g n s

-- roll n number of s sided dice dice, removing either highest or lowest r elements
rollAndRemoveDice :: StdGen -> Int -> Int -> Int -> Bool -> ErrorProne (Int, StdGen)
rollAndRemoveDice g n s r t = first sum <$> (getDiceRoll g n s >>= (\d -> (, snd d) <$> removeDice (fst d) r t))

-- remove highest n elements from list if t is true
-- otherwise remove lowest n elements
-- return Nothing if n is negative or attempting to remove more elements than exist
removeDice :: [Int] -> Int -> Bool -> ErrorProne [Int]
removeDice [] _ _ = Right []
removeDice l n t
  | n < 0 = Left "cannot remove a negative number of dice"
  | n == 0 = Right l
  | t = reverse <$> removeSortedDice (reverse (sort l)) n
  | otherwise = removeSortedDice (sort l) n

-- drop n elements from beginning of list
-- return Nothing if n is negative or attempting to remove more elements than exist
removeSortedDice :: [Int] -> Int -> ErrorProne [Int]
removeSortedDice l 0 = Right l
removeSortedDice [] _ = Left "cannot remove more dice than were rolled "
removeSortedDice l n
  | n < 0 = Left "cannot remove a negative number of dice"
  | otherwise = Right $ drop n l