import Error
import Evaluator
import Parser
import System.IO (hFlush, stdout)
import System.Random
import Tokenizer

-- TODO:
--  improve error messege clarity
--  bcd float encoding???

calc :: String -> StdGen -> ErrorProne Double
calc = evaluate . parse . tokenize

calcToString :: ErrorProne Double -> String
calcToString (Left e) = "error: " ++ e
calcToString (Right n) = show n

getAnswer :: String -> StdGen -> String
getAnswer gen input = calcToString $ calc gen input

showAnswer :: String -> StdGen -> IO()
showAnswer input gen = do
      putStrLn $ getAnswer input gen
      newGen <- newStdGen
      inputLoop input newGen

inputLoop :: String -> StdGen -> IO ()
inputLoop pInput gen = do
  putStr "Enter Value: "
  hFlush stdout
  input <- getLine
  case input of
    "exit" -> putStrLn "exiting"
    "quit" -> putStrLn "exiting"
    "" -> showAnswer pInput gen
    _ -> showAnswer input gen

main :: IO ()
main = do
  gen <- getStdGen
  inputLoop "" gen