import Data.Bifunctor (first, second)
import Data.Char (toLower)
import Error
import Evaluator
import Options.Applicative
import Parser
import System.Console.ANSI
import System.Console.Haskeline
import System.IO (hFlush, stdout)
import System.Random
import Text.Read (readMaybe)
import Tokenizer

-- TODO:
--  improve error messege clarity
--  bcd float encoding???
--  pecentge dice (d%)
--  repeated rolls ([n]x[roll])

data Options = Options
  { optColor :: Bool,
    optBold :: Bool,
    optClear :: Bool,
    optGen :: Maybe Int,
    optEval :: Maybe String
  }

toIntegral :: (RealFrac a, Integral b) => a -> Maybe b
toIntegral r
  | fromIntegral i == r = Just i
  | otherwise = Nothing
  where
    i = truncate r

applyIfTrue :: (a -> a) -> Bool -> a -> a
applyIfTrue f True x = f x
applyIfTrue _ False x = x

boldText :: Options -> String -> String
boldText o =
  applyIfTrue
    (styleText [SetConsoleIntensity BoldIntensity])
    (optBold o)

colorText :: Options -> ColorIntensity -> Color -> String -> String
colorText o i c =
  applyIfTrue
    (styleText [SetColor Foreground i c])
    (optColor o)

calculate :: String -> StdGen -> (ErrorProne Double, StdGen)
calculate = evaluate . parse . tokenize

calcToString :: Options -> ErrorProne Double -> String
calcToString o (Left e) =
  colorText o Dull Red (boldText o "error: ") ++ e
calcToString o (Right n) = case toIntegral n of
  Nothing -> boldText o $ show n
  Just i -> boldText o $ show i

getAnswer :: Options -> String -> StdGen -> (String, StdGen)
getAnswer o input gen = first (calcToString o) (calculate (shortcuts input) gen)

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
  gen <- getGen o
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
      _ ->
        let ans = getAnswer o input gen
         in do
              outputStrLn $ fst ans
              loop o (snd ans)

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
    <*> option
      (return <$> maybeReader readMaybe)
      ( long "rand"
          <> short 'r'
          <> metavar "INT"
          <> value Nothing
          <> help "Use a specific pseudorandom number generator for deterministic results"
      )
    <*> option
      (return <$> str)
      ( long "eval"
          <> short 'e'
          <> metavar "EXPRESSION"
          <> value Nothing
          <> help "Evaluate a specific expression, skipping the interactive prompt"
      )

clearWrapper :: IO () -> IO ()
clearWrapper f = do
  clearScreen
  setCursorPosition 0 0
  f
  clearScreen
  setCursorPosition 0 0

getGen :: Options -> IO StdGen
getGen o = case optGen o of
  Nothing -> newStdGen
  Just i -> pure $ mkStdGen i

parseArgs :: Options -> IO ()
parseArgs o
  | optClear o = case optEval o of
      Nothing -> clearWrapper $ parseArgs o {optClear = False}
      Just _ -> parseArgs o {optClear = False}
  | otherwise = case optEval o of
      Nothing -> interactive o
      Just input -> do
        gen <- getGen o
        putStrLn $ fst $ getAnswer o (map toLower input) gen

main :: IO ()
main = parseArgs =<< execParser opts
  where
    opts =
      info
        (helper <*> options)
        ( fullDesc
            <> progDesc "Roll dice and do math"
        )
