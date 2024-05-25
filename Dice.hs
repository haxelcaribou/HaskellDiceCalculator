import System.Random

--  TODO: Add dice removal 

rollDice :: StdGen -> Int -> Int -> Maybe [Int]
rollDice g n s
  | n < 0 = Nothing
  | otherwise = Just $ take n $ randomRs (1, s) g

removeDice :: [Int] -> Int -> Bool -> Maybe [Int]
removeDice [] _ _ = Just []
removeDice l n t
  | n < 0 = Nothing
  | n == 0 = Just l
  | otherwise = error "Not yet implemented"