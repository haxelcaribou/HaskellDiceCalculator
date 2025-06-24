module Dice (rollDice, rollAndRemoveDice) where

import Data.Bifunctor
import Data.List (sort)
import Error
import System.Random
import Test.QuickCheck

sequenceFst :: (ErrorProne a, b) -> ErrorProne (a, b)
sequenceFst (Left e, _) = Left e
sequenceFst (Right a, b) = Right (a, b)

-- return list of n number of s sided dice and new StdGen
-- return an Error if rolling negative number of dice
--  or rolling dice with a non-positive number of sides
getDiceRoll :: Int -> Int -> StdGen -> (ErrorProne [Int], StdGen)
getDiceRoll n s g
  | n < 0 = (Left "cannot roll a negative number of dice", g)
  | s <= 0 = (Left "dice must have a positive number of sides", g)
  | n == 0 || s == 0 = (Right [], g)
  | otherwise =
      let r = randomR (1, s) g
       in first (fmap (fst r :)) $ getDiceRoll (n - 1) s (snd r)

-- roll n number of s sided dice
rollDice :: Int -> Int -> StdGen -> (ErrorProne Int, StdGen)
rollDice n s = first (fmap sum) . getDiceRoll n s

-- roll n number of s sided dice dice, removing either highest or lowest r elements
rollAndRemoveDice :: Int -> Int -> Int -> Bool -> StdGen -> (ErrorProne Int, StdGen)
rollAndRemoveDice n s r t = first (fmap sum . (>>= removeDice r t)) . getDiceRoll n s

-- remove highest n elements from list if t is true
-- otherwise remove lowest n elements
removeDice :: Int -> Bool -> [Int] -> ErrorProne [Int]
removeDice n t l
  | t = reverse <$> removeSortedDice n (reverse (sort l))
  | otherwise = removeSortedDice n (sort l)

-- drop n elements from beginning of list
-- return an Error if removing a negative number of elements or removing more elements than exist
removeSortedDice :: Int -> [Int] -> ErrorProne [Int]
removeSortedDice 0 l = Right l
removeSortedDice _ [] = Left "cannot remove more dice than were rolled"
removeSortedDice n l
  | n < 0 = Left "cannot remove a negative number of dice"
  | n > length l = Left "cannot remove more dice than were rolled"
  | otherwise = Right $ drop n l

-- Tests

prop_getDiceRollError (n, s, gi) =
  (n < 0 || s <= 0)
    == case fst $ getDiceRoll n s (mkStdGen gi) of Left _ -> True; _ -> False

prop_removeSortedDiceError (n, l) =
  (n < 0 || n > length l)
    == case removeSortedDice n l of Left _ -> True; _ -> False

prop_rollAndRemoveDiceError (n, s, r, t, gi) =
  (n < 0 || s <= 0 || r < 0 || r > n)
    == case fst $ rollAndRemoveDice n s r t (mkStdGen gi) of Left _ -> True; _ -> False
