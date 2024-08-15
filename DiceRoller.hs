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
  runInputT (setComplete noCompletion defaultSettings)  (loop gen)
  where
    loop :: StdGen -> InputT IO ()
    loop gen = do
      minput <- getInputLine "Enter Value: "
      case minput of
        Nothing -> return ()
        Just "" -> loop gen
        Just "exit" -> return ()
        Just "quit" -> return ()
        Just input -> do
          outputStrLn $ getAnswer input gen
          newGen <- newStdGen
          loop newGen