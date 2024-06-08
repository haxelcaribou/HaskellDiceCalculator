{-# LANGUAGE TupleSections #-}
module Evaluator (evaluate) where

import Dice
import Error
import System.Random
import Token
import Tree
import Data.Bifunctor (first, second)

sequenceFst :: (ErrorProne a, b) -> ErrorProne (a , b)
sequenceFst (Left e, _) = Left e
sequenceFst (Right a, b) = Right (a, b)

errorMessege :: Maybe a -> String -> ErrorProne a
errorMessege m s = maybe (Left s) Right m

toIntegral :: (RealFrac a, Integral b) => a -> Maybe b
toIntegral r
  | fromIntegral i == r = Just i
  | otherwise = Nothing
  where
    i = truncate r

mergeMaybe :: Maybe (Maybe a) -> Maybe a
mergeMaybe Nothing = Nothing
mergeMaybe (Just Nothing) = Nothing
mergeMaybe (Just (Just x)) = Just x

intFuncSingle :: (Integral a) => (a -> a) -> (Double -> Maybe Double)
intFuncSingle f x = fromIntegral . f <$> toIntegral x

intFuncSingle' :: (Integral a) => (a -> Maybe a) -> (Double -> Maybe Double)
intFuncSingle' f x = fromIntegral <$> (toIntegral x >>= f)

intFuncDouble :: (Integral a) => (a -> a -> a) -> (Double -> Double -> Maybe Double)
intFuncDouble f a b = fromIntegral <$> (f <$> toIntegral a <*> toIntegral b)

intFuncDouble' :: (Integral a) => (a -> a -> Maybe a) -> (Double -> Double -> Maybe Double)
intFuncDouble' f a b = fromIntegral <$> mergeMaybe (f <$> toIntegral a <*> toIntegral b)

mod' :: Integral a => a -> a -> Maybe a
mod' _ 0 = Nothing
mod' a b = Just $ mod a b

rem' :: Integral a => a -> a -> Maybe a
rem' _ 0 = Nothing
rem' a b = Just $ rem a b

fac :: Integral a => a -> Maybe a
fac x
  | x < 0 = Nothing
  | x == 0 = Just 1
  | otherwise = (x *) <$> fac (x - 1)

aDefault :: Double -> StdGen -> ErrorProne (Double, StdGen)
aDefault x g = Right (x, g)

applyDice :: Maybe Int -> Maybe Int -> StdGen -> ErrorProne (Double, StdGen)
applyDice Nothing _ g = Left "dice input must be an integer"
applyDice _ Nothing g = Left "dice input must be an integer"
applyDice (Just num) (Just sides) g = first fromIntegral <$> Dice.rollDice num sides g

applyOperator :: Char -> [Double] -> StdGen -> ErrorProne (Double, StdGen)
applyOperator _ [] g = Left "too few operands"
applyOperator _ (a : b : c : xs) g = Left "too many operands"
applyOperator o [a, b] g
  | o == '+' = aDefault (a + b) g
  | o == '-' = aDefault (a - b) g
  | o == '*' = aDefault (a * b) g
  | o == '/' = aDefault (a / b) g
  | o == '^' = aDefault (a ** b) g
  | o == '%' = (, g) <$> errorMessege (intFuncDouble' mod' a b) "mod input must be an integer with a nonzero dividend"
  | o == 'd' = applyDice (toIntegral a) (toIntegral b) g
  | otherwise = Left "unknown operator"
applyOperator o [a] g
  | o == '+' = aDefault a g
  | o == '-' = aDefault (-a) g
  | o == '~' = aDefault (-a) g
  | o == 'd' = applyDice (Just 1) (toIntegral a) g
  | o == '!' = (, g) <$> errorMessege (intFuncSingle' fac a) "factorial input must be a positive integer"

applyFunction :: String -> [Double] -> StdGen -> ErrorProne (Double, StdGen)
applyFunction o [] g
  | o == "pi" = aDefault pi g
  | o == "e" = aDefault (exp 1) g
  | otherwise = Left "unknown constant"
applyFunction o [x] g
  | o == "negate" = aDefault (negate x) g
  | o == "abs" = aDefault (abs x) g
  | o == "sign" = aDefault (signum x) g
  | o == "round" = aDefault (fromIntegral $ round x) g
  | o == "trunc" = aDefault (fromIntegral $ truncate x) g
  | o == "floor" = aDefault (fromIntegral $ floor x) g
  | o == "ceil" = aDefault (fromIntegral $ ceiling x) g
  | o == "exp" = aDefault (exp x) g
  | o == "sqrt" = aDefault (sqrt x) g
  | o == "ln" = aDefault (log x) g
  | o == "log" = aDefault (logBase 10 x) g
  | o == "fac" = (, g) <$> errorMessege (intFuncSingle' fac x) "factorial input must be a positive integer"
  | o == "sin" = aDefault (sin x) g
  | o == "tan" = aDefault (tan x) g
  | o == "cos" = aDefault (cos x) g
  | o == "asin" = aDefault (asin x) g
  | o == "atan" = aDefault (atan x) g
  | o == "acos" = aDefault (acos x) g
  | o == "rad" = aDefault (x / 180 * pi) g
  | o == "deg" = aDefault (x / pi * 180) g
applyFunction o [a, b] g
  | o == "add" = aDefault (a + b) g
  | o == "sub" = aDefault (a - b) g
  | o == "mult" = aDefault (a * b) g
  | o == "div" = aDefault (a / b) g
  | o == "mod" = (, g) <$> errorMessege (intFuncDouble' mod' a b) "mod input must be an integer with a nonzero dividend"
  | o == "rem" = (, g) <$> errorMessege (intFuncDouble' rem' a b) "mod input must be an integer with a nonzero dividend"
  | o == "pow" = aDefault (a ** b) g
  | o == "log" = aDefault (logBase b a) g
applyFunction o l@(x : xs) g
  | o == "sum" = aDefault (sum l) g
  | o == "prod" = aDefault (product l) g
  | o == "min" = aDefault (minimum l) g
  | o == "max" = aDefault (maximum l) g
  | otherwise = Left "unknown function"

evaluate :: ErrorProne (Tree Token) -> StdGen -> ErrorProne Double
evaluate t g = fst <$> evaluate' t g

evaluate' :: ErrorProne (Tree Token) -> StdGen -> ErrorProne (Double, StdGen)
evaluate' (Left e) _ = Left e
evaluate' (Right (Leaf x)) g = case x of
  Number n -> Right (n, g)
  _ -> Left "bad input"
evaluate' (Right (Branch x l)) g = case x of
  Operator c -> evaluateList l g >>= uncurry (applyOperator c)
  Function s -> evaluateList l g >>= uncurry (applyFunction s)
  _ -> Left "bad input"

evaluateList :: [Tree Token] -> StdGen -> ErrorProne ([Double], StdGen)
evaluateList [] g = Right ([], g)
evaluateList l@(x : xs) g = do
  a <- evaluate' (Right x) g
  b <- evaluateList xs (snd a)
  Right (first (fst a :) b)