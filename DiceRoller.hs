import Error
import Evaluator
import Parser
import System.IO (hFlush, stdout)
import System.Random
import Tokenizer

-- TODO:
--  commas in numbers!
--  improve error messege clarity
--  bcd float encoding???
--  add postfix operators (just ! I think)

calc :: StdGen -> String -> ErrorProne Double
calc g = evaluate g . parse . tokenize

calcToString :: ErrorProne Double -> String
calcToString (Left e) = e
calcToString (Right n) = show n

answer :: StdGen -> IO ()
answer gen = do
  putStr "Enter Value: "
  hFlush stdout
  input <- getLine
  case input of
    "exit" -> print "exiting"
    _ -> do
      print . calcToString . calc gen $ input
      newGen <- newStdGen
      answer newGen

main :: IO ()
main = do
  gen <- getStdGen
  answer gen