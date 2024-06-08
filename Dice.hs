module Dice (rollDice) where

import Data.Bifunctor
import Data.List (sort)
import Error
import System.Random

sequenceFst :: (ErrorProne a, b) -> ErrorProne (a , b)
sequenceFst (Left e, _) = Left e
sequenceFst (Right a, b) = Right (a, b)

-- return list of n number of s sided dice and new StdGen
-- return Nothing if rolling negative number of dice or with negative number of sides
getDiceRoll :: Int -> Int -> StdGen -> ErrorProne ([Int], StdGen)
getDiceRoll n s g
  | n < 0 = Left "cannot roll a negative number of dice"
  | s < 0 = Left "dice must have a positive number of sides"
  | n == 0 || s == 0 = Right ([], g)
  | otherwise =
      let r = randomR (1, s) g
       in first (fst r :) <$> getDiceRoll (n - 1) s (snd r)

-- roll n number of s sided dice
rollDice :: Int -> Int -> StdGen -> ErrorProne (Int, StdGen)
rollDice n s g = first sum <$> getDiceRoll n s g

-- roll n number of s sided dice dice, removing either highest or lowest r elements
rollAndRemoveDice :: Int -> Int -> Int -> Bool -> StdGen -> ErrorProne (Int, StdGen)
rollAndRemoveDice n s r t g = first sum <$> (getDiceRoll n s g >>= (sequenceFst . first (removeDice r t)))

-- remove highest n elements from list if t is true
-- otherwise remove lowest n elements
-- return Nothing if n is negative or attempting to remove more elements than exist
removeDice :: Int -> Bool -> [Int] -> ErrorProne [Int]
removeDice _ _ [] = Right []
removeDice n t l
  | n < 0 = Left "cannot remove a negative number of dice"
  | n == 0 = Right l
  | t = reverse <$> removeSortedDice n (reverse (sort l))
  | otherwise = removeSortedDice n (sort l)

-- drop n elements from beginning of list
-- return Nothing if n is negative or attempting to remove more elements than exist
removeSortedDice :: Int -> [Int] -> ErrorProne [Int]
removeSortedDice 0 l = Right l
removeSortedDice _ [] = Left "cannot remove more dice than were rolled "
removeSortedDice n l
  | n < 0 = Left "cannot remove a negative number of dice"
  | otherwise = Right $ drop n l