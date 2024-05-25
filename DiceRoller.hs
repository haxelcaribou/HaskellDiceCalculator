{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use second" #-}

import Error
import Token
import Tree
import Tokenizer
import Parser
import Evaluator

-- TODO:
--  add dice rolls
--  add mod (%)
--  add postfix operators (just ! I think) (update: the gamma function is actually pretty complicated)
--  remove errors and change to either for evaluation
--  multiple Number types for integer precision? this is probably a bad idea

calc :: String -> Double
calc = evaluate . parse . tokenize

-- main :: IO ()
main = do
  putStr "Enter Value: "
  input <- getLine
  print . calc $ input