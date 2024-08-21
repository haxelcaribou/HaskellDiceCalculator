import Data.Char (toLower)
import Error
import Evaluator
import Options.Applicative
import Parser
import System.Console.ANSI
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

calcToString :: Options -> ErrorProne Double -> String
calcToString o (Left e) = applyIfTrue (styleText [SetColor Foreground Dull Red]) (not $ noColor o) (applyIfTrue (styleText [SetConsoleIntensity BoldIntensity]) (bold o) "error: ") ++ e
calcToString o (Right n) = applyIfTrue (styleText [SetConsoleIntensity BoldIntensity]) (bold o) $ show n

getAnswer :: Options -> String -> StdGen -> String
getAnswer o gen input = calcToString o $ calc gen input

styleText :: [SGR] -> String -> String
styleText style text = setSGRCode style ++ text ++ setSGRCode [Reset]

applyIfTrue :: (a -> a) -> Bool -> a -> a
applyIfTrue f True x = f x
applyIfTrue _ False x = x

interactive :: Options -> IO ()
interactive o = do
  gen <- getStdGen
  runInputT (setComplete noCompletion defaultSettings) (loop o gen)
  where
    loop :: Options -> StdGen -> InputT IO ()
    loop o gen = do
      minput <- getInputLine $ applyIfTrue (styleText [SetColor Foreground Vivid Blue]) (not $ noColor o) "Enter Value: "
      case minput of
        Nothing -> return ()
        Just input -> takeInput o gen $ map toLower input

    takeInput :: Options -> StdGen -> String -> InputT IO ()
    takeInput o gen input = case input of
      "" -> loop o gen
      "exit" -> return ()
      "quit" -> return ()
      "t" -> do
        outputStrLn $ getAnswer o "1d20" gen
        newGen <- newStdGen
        loop o newGen
      "a" -> do
        outputStrLn $ getAnswer o "2d20b1" gen
        newGen <- newStdGen
        loop o newGen
      "d" -> do
        outputStrLn $ getAnswer o "2d20t1" gen
        newGen <- newStdGen
        loop o newGen
      "s" -> do
        outputStrLn $ getAnswer o "4d6b1" gen
        newGen <- newStdGen
        loop o newGen
      input -> do
        outputStrLn $ getAnswer o input gen
        newGen <- newStdGen
        loop o newGen

data Options = Options
  { noColor :: Bool,
    bold :: Bool,
    clear :: Bool
  }

options :: Parser Options
options =
  Options
    <$> switch
      ( long "no-color"
          <> short 'n'
          <> help "Don't color output"
      )
    <*> switch
      ( long "bold"
          <> short 'b'
          <> help "Output final answers in bold"
      )
    <*> switch
      ( long "clear"
          <> short 'c'
          <> help "Clear screen before and after running"
      )

main :: IO ()
main = parseArgs =<< execParser opts
  where
    opts =
      info
        (options <**> helper)
        ( fullDesc
            <> progDesc "Roll dice and do math"
        )

parseArgs :: Options -> IO ()
parseArgs (Options noColor bold True) = do
  clearScreen
  setCursorPosition 0 0
  parseArgs (Options noColor bold False)
  clearScreen
  setCursorPosition 0 0
parseArgs options@(Options noColor bold False) = interactive options