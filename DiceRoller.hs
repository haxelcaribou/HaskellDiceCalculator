import Error
import Tokenizer
import Parser
import Evaluator
import System.Random

-- TODO:
--  make dice be actualy random
--  improve error messege clarity
--  bcd float encoding???
--  add mod (%)
--  add postfix operators (just ! I think)

calc :: StdGen -> String -> ErrorProne Double
calc s = evaluate s . parse . tokenize

calcToString :: ErrorProne Double -> String
calcToString (Left e) = e
calcToString (Right n) = show n

main :: IO ()
main = do
  putStr "Enter Value: "
  input <- getLine
  gen <- getStdGen
  print . calcToString . calc gen $ input