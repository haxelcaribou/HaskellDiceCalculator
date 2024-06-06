import Error
import Evaluator
import Parser
import System.Random
import Tokenizer

-- TODO:
--  make dice be actually random (oh no IO)
--  improve error messege clarity
--  bcd float encoding???
--  add postfix operators (just ! I think)

calc :: StdGen -> String -> ErrorProne Double
calc g s = fst <$> (evaluate g . parse . tokenize) s

calcToString :: ErrorProne Double -> String
calcToString (Left e) = e
calcToString (Right n) = show n

answer :: StdGen -> IO ()
answer gen = do
  putStr "Enter Value: "
  input <- getLine
  if input == "exit"
    then print "exiting"
    else do
      print . calcToString . calc gen $ input
      newGen <- newStdGen
      answer newGen

main :: IO ()
main = do
  gen <- getStdGen
  answer gen