{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use second" #-}

import Error
import Token
import Tree
import Tokenizer
import Parser

type TokenTree = Tree Token

-- TODO:
--  add dice rolls
--  add mod (%)
--  add postfix operators (just ! I think) (update: the gamma function is actually pretty complicated)
--  remove errors and change to either for evaluation
--  multiple Number types for integer precision? this is probably a bad idea

errorMessege :: Maybe a -> String -> Either Error a
errorMessege Nothing e = Left e
errorMessege (Just x) _ = Right x

toIntegral :: (RealFrac a, Integral b) => a -> Maybe b
toIntegral r
  | fromIntegral i == r = Just i
  | otherwise = Nothing
  where i = truncate r

fac :: Integral a => a -> Maybe a
fac x
  | x < 0 = Nothing
  | x == 0 = Just 1
  | otherwise = fac (x - 1) >>= \a -> Just (x * a)

applyOperator :: Char -> [Double] -> Double
applyOperator _ [] = error "too few operands"
applyOperator _ (a : b : c : xs) = error "too many operands"
applyOperator o [a, b]
  | o == '+' = a + b
  | o == '-' = a - b
  | o == '*' = a * b
  | o == '/' = a / b
  | o == '^' = a ** b
  | otherwise = error "unknown operator"
applyOperator o [a]
  | o == '+' = a
  | o == '-' = -a
  | o == '~' = -a
  -- | o == '!' = fac $ toIntegral a
  | otherwise = error "unknown unary operator"

applyFunction :: String -> [Double] -> Double
applyFunction o []
  | o == "pi" = pi
  | o == "e" = exp 1
  | otherwise = error "unknown constant"
applyFunction o [x]
  | o == "negate" = negate x
  | o == "abs" = abs x
  | o == "sign" = signum x
  | o == "round" = fromIntegral $ round x
  | o == "trunc" = fromIntegral $ truncate x
  | o == "floor" = fromIntegral $ floor x
  | o == "ceil" = fromIntegral $ ceiling x
  | o == "exp" = exp x
  | o == "sqrt" = sqrt x
  | o == "ln" = log x
  | o == "log" = logBase 10 x
  -- | o == "fac" = fac $ toIntegral x
  | o == "sin" = sin x
  | o == "tan" = tan x
  | o == "cos" = cos x
  | o == "asin" = asin x
  | o == "atan" = atan x
  | o == "acos" = acos x
  | o == "rad" = pi / 180 * x
  | o == "deg" = 180 / pi * x
applyFunction o [a, b]
  | o == "add" = a + b
  | o == "sub" = a - b
  | o == "mult" = a * b
  | o == "div" = a / b
  | o == "pow" = a ** b
  | o == "log" = logBase b a
applyFunction o l@(x : xs)
  | o == "sum" = sum l
  | o == "prod" = product l
  | o == "min" = minimum l
  | o == "max" = maximum l
  | otherwise = error "unknown function"

evaluate :: TokenTree -> Double
evaluate (Leaf x) = case x of
  Number n -> n
  _ -> error "bad input"
evaluate (Branch x l) = case x of
  Operator c -> applyOperator c $ map evaluate l
  Function s -> applyFunction s $ map evaluate l
  _ -> error "bad input"

calc :: String -> Double
calc = evaluate . parse . tokenize

-- main :: IO ()
main = do
  putStr "Enter Value: "
  input <- getLine
  print . calc $ input