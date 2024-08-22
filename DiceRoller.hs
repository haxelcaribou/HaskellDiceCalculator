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

data Options = Options
  { color :: Bool,
    bold :: Bool,
    clear :: Bool,
    eval :: Maybe String
  }

applyIfTrue :: (a -> a) -> Bool -> a -> a
applyIfTrue f True x = f x
applyIfTrue _ False x = x

boldText :: Options -> String -> String
boldText o =
  applyIfTrue
    (styleText [SetConsoleIntensity BoldIntensity])
    (bold o)

colorText :: Options -> ColorIntensity -> Color -> String -> String
colorText o i c =
  applyIfTrue
    (styleText [SetColor Foreground i c])
    (color o)

calc :: String -> StdGen -> ErrorProne Double
calc = evaluate . parse . tokenize

calcToString :: Options -> ErrorProne Double -> String
calcToString o (Left e) =
  colorText o Dull Red (boldText o "error: ") ++ e
calcToString o (Right n) = boldText o $ show n

getAnswer :: Options -> String -> StdGen -> String
getAnswer o input gen = calcToString o $ calc (shortcuts input) gen

shortcuts :: String -> String
shortcuts "t" = "1d20"
shortcuts "a" = "2d20b1"
shortcuts "d" = "2d20t1"
shortcuts "s" = "4d6b1"
shortcuts input = input

styleText :: [SGR] -> String -> String
styleText style text = setSGRCode style ++ text ++ setSGRCode [Reset]

interactive :: Options -> IO ()
interactive o = do
  gen <- getStdGen
  runInputT (setComplete noCompletion defaultSettings) (loop o gen)
  where
    loop :: Options -> StdGen -> InputT IO ()
    loop o gen = do
      minput <-
        getInputLine $ colorText o Vivid Blue "Enter Value: "
      case minput of
        Nothing -> return ()
        Just input -> takeInput o gen $ map toLower input

    takeInput :: Options -> StdGen -> String -> InputT IO ()
    takeInput o gen input = case input of
      "" -> loop o gen
      "exit" -> return ()
      "quit" -> return ()
      _ -> do
        outputStrLn $ getAnswer o input gen
        newGen <- newStdGen
        loop o newGen

options :: Parser Options
options =
  Options
    <$> flag
      True
      False
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
    <*> option (return <$> str)
      (
        long "eval"
          <> short 'e'
          <> metavar "EXPRESSION"
          <> value Nothing
          <> help "Evaluate a specific expression, skipping the interactive prompt"
      )

parseArgs :: Options -> IO ()
parseArgs (Options color bold True Nothing) = do
  clearScreen
  setCursorPosition 0 0
  parseArgs (Options color bold False Nothing)
  clearScreen
  setCursorPosition 0 0
parseArgs o@(Options color bold False Nothing) = interactive o
parseArgs (Options color bold True (Just input)) = do
  clearScreen
  setCursorPosition 0 0
  parseArgs (Options color bold False (Just input))
  clearScreen
  setCursorPosition 0 0
parseArgs o@(Options color bold False (Just input)) = do
  gen <- getStdGen
  putStrLn $ getAnswer o (map toLower input) gen


main :: IO ()
main = parseArgs =<< execParser opts
  where
    opts =
      info
        (helper <*> options)
        ( fullDesc
            <> progDesc "Roll dice and do math"
        )