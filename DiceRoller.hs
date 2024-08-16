import Data.Char (toLower)
import Error
import Evaluator
import Parser
import System.Console.Haskeline
import System.IO (hFlush, stdout)
import System.Random
import Tokenizer

-- TODO:
--  improve error messege clarity
--  bcd float encoding???
--  pecentge dice (d%)
--  repeated rolls ([n]x[roll])

calc :: String -> StdGen -> ErrorProne Double
calc = evaluate . parse . tokenize

calcToString :: ErrorProne Double -> String
calcToString (Left e) = "error: " ++ e
calcToString (Right n) = show n

getAnswer :: String -> StdGen -> String
getAnswer gen input = calcToString $ calc gen input

main :: IO ()
main = do
  gen <- getStdGen
  runInputT (setComplete noCompletion defaultSettings) (loop gen)
  where
    loop :: StdGen -> InputT IO ()
    loop gen = do
      minput <- getInputLine "Enter Value: "
      case minput of
        Nothing -> return ()
        Just input -> takeInput gen $ map toLower input

    takeInput :: StdGen -> String -> InputT IO ()
    takeInput gen input = case input of
      "" -> loop gen
      "exit" -> return ()
      "quit" -> return ()
      "t" -> do
        outputStrLn $ getAnswer "1d20" gen
        newGen <- newStdGen
        loop newGen
      "a" -> do
        outputStrLn $ getAnswer "2d20b1" gen
        newGen <- newStdGen
        loop newGen
      "d" -> do
        outputStrLn $ getAnswer "2d20t1" gen
        newGen <- newStdGen
        loop newGen
      "s" -> do
        outputStrLn $ getAnswer "4d6b1" gen
        newGen <- newStdGen
        loop newGen
      input -> do
        outputStrLn $ getAnswer input gen
        newGen <- newStdGen
        loop newGen