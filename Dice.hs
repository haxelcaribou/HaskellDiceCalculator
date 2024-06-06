{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use first" #-}

module Dice (rollDice) where

import System.Random

--  TODO: Add dice removal

rollDice :: StdGen -> Int -> Int -> Maybe (Int, StdGen)
rollDice g n s =
  let dMaybe = getDiceRoll g n s
   in case dMaybe of
        Nothing -> Nothing
        Just d -> Just (sum (fst d), snd d)

getDiceRoll :: StdGen -> Int -> Int -> Maybe ([Int], StdGen)
getDiceRoll g n s
  | n < 0 = Nothing
  | n == 0 = Just ([], g)
  | otherwise =
      let r = randomR (1, s) g
          dMaybe = getDiceRoll (snd r) (n - 1) s
       in case dMaybe of
            Nothing -> Nothing
            Just d -> Just (fst r : fst d, snd d)

removeDice :: [Int] -> Int -> Bool -> Maybe [Int]
removeDice [] _ _ = Just []
removeDice l n t
  | n < 0 = Nothing
  | n == 0 = Just l
  | otherwise = error "Not yet implemented"