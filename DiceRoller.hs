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
--  multiple parenthesis types
--  add postfix operators (just ! I think)

calc :: String -> StdGen -> ErrorProne Double
calc = evaluate . parse . tokenize

calcToString :: ErrorProne Double -> String
calcToString (Left e) = "error: " ++ e
calcToString (Right n) = show n

answer :: StdGen -> IO ()
answer gen = do
  putStr "Enter Value: "
  hFlush stdout
  input <- getLine
  case input of
    "exit" -> putStrLn "exiting"
    "quit" -> putStrLn "exiting"
    _ -> do
      putStrLn $ calcToString $ calc input gen
      newGen <- newStdGen
      answer newGen

main :: IO ()
main = do
  gen <- getStdGen
  answer gen