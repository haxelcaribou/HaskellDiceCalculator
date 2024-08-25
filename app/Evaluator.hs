{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant evaluate" #-}

module Evaluator (evaluate) where

import Data.Bifunctor (first, second)
import Dice
import Error
import System.Random
import Token
import Tree

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

aDefault :: Double -> StdGen -> (ErrorProne Double, StdGen)
aDefault x g = (Right x, g)

aError :: String -> StdGen -> (ErrorProne Double, StdGen)
aError s g = (Left s, g)

applyDice :: Maybe Int -> Maybe Int -> StdGen -> (ErrorProne Double, StdGen)
applyDice Nothing _ = aError "dice input must be an integer"
applyDice _ Nothing = aError "dice input must be an integer"
applyDice (Just n) (Just s) = first (fmap fromIntegral) . rollDice n s

applyDiceRemove :: Maybe Int -> Maybe Int -> Maybe Int -> Bool -> StdGen -> (ErrorProne Double, StdGen)
applyDiceRemove Nothing _ _ _ = aError "dice input must be an integer"
applyDiceRemove _ Nothing _ _ = aError "dice input must be an integer"
applyDiceRemove _ _ Nothing _ = aError "dice input must be an integer"
applyDiceRemove (Just n) (Just s) (Just r) t = first (fmap fromIntegral) . rollAndRemoveDice n s r t

applyOperator :: Char -> [Double] -> StdGen -> (ErrorProne Double, StdGen)
applyOperator _ [] g = aError "too few operands" g
applyOperator o [a, b, c] g
  | o == 't' = applyDiceRemove (toIntegral a) (toIntegral b) (toIntegral c) True g
  | o == 'b' = applyDiceRemove (toIntegral a) (toIntegral b) (toIntegral c) False g
applyOperator o [a, b] g
  | o == '+' = aDefault (a + b) g
  | o == '-' = aDefault (a - b) g
  | o == '*' = aDefault (a * b) g
  | o == '/' = aDefault (a / b) g
  | o == '^' = aDefault (a ** b) g
  | o == '%' = (,g) $ errorMessege (intFuncDouble' mod' a b) "mod input must be integers with a nonzero dividend"
  | o == 'd' = applyDice (toIntegral a) (toIntegral b) g
  | otherwise = aError "unknown operator" g
applyOperator o [a] g
  | o == '+' = aDefault a g
  | o == '-' = aDefault (-a) g
  | o == '~' = aDefault (-a) g
  | o == 'd' = applyDice (Just 1) (toIntegral a) g
  | o == '%' = applyDice (toIntegral a) (Just 100) g
  | o == '!' = (,g) $ errorMessege (intFuncSingle' fac a) "factorial input must be a positive integer"
applyOperator _ xs g = aError "too many operands" g

applyFunction :: String -> [Double] -> StdGen -> (ErrorProne Double, StdGen)
applyFunction o [] g
  | o == "pi" = aDefault pi g
  | o == "tau" = aDefault (pi * 2) g
  | o == "e" = aDefault (exp 1) g
  | otherwise = aError "unknown constant" g
applyFunction o [x] g
  | o == "negate" = aDefault (negate x) g
  | o == "abs" = aDefault (abs x) g
  | o == "sign" = aDefault (signum x) g
  | o == "round" = aDefault (fromIntegral $ round x) g
  | o == "trunc" = aDefault (fromIntegral $ truncate x) g
  | o == "floor" = aDefault (fromIntegral $ floor x) g
  | o == "ceil" = aDefault (fromIntegral $ ceiling x) g
  | o == "inv" = aDefault (x ** (-1)) g
  | o == "exp" = aDefault (exp x) g
  | o == "sqrt" = aDefault (sqrt x) g
  | o == "ln" = aDefault (log x) g
  | o == "log" = aDefault (logBase 10 x) g
  | o == "fac" = (,g) $ errorMessege (intFuncSingle' fac x) "factorial input must be a positive integer"
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
  | o == "mod" = (,g) $ errorMessege (intFuncDouble' mod' a b) "mod input must be integers with a nonzero dividend"
  | o == "rem" = (,g) $ errorMessege (intFuncDouble' rem' a b) "mod input must be integers with a nonzero dividend"
  | o == "pow" = aDefault (a ** b) g
  | o == "log" = aDefault (logBase b a) g
applyFunction o l@(x : xs) g
  | o == "sum" = aDefault (sum l) g
  | o == "prod" = aDefault (product l) g
  | o == "min" = aDefault (minimum l) g
  | o == "max" = aDefault (maximum l) g
  | otherwise = aError "unknown function" g

-- evaluate :: ErrorProne (Tree Token) -> StdGen -> ErrorProne Double
-- evaluate t g = fst <$> evaluate' t g

evaluate :: ErrorProne (Tree Token) -> StdGen -> (ErrorProne Double, StdGen)
evaluate (Left e) g = (Left e, g)
evaluate (Right (Leaf x)) g = case x of
  Number n -> (Right n, g)
  _ -> aError "bad input" g
evaluate (Right (Branch x l)) g = case x of
  Operator c -> applyList (applyOperator c) (evaluateList l g)
  Function s -> applyList (applyFunction s) (evaluateList l g)
  _ -> aError "bad input" g
  where
    applyList :: ([Double] -> StdGen -> (ErrorProne Double, StdGen)) -> (ErrorProne [Double], StdGen) -> (ErrorProne Double, StdGen)
    applyList f (Left e, g) = (Left e, g)
    applyList f (Right xs, g) = f xs g

evaluateList :: [Tree Token] -> StdGen -> (ErrorProne [Double], StdGen)
evaluateList [] g = (Right [], g)
evaluateList l@(x : xs) g =
  let a = evaluate (Right x) g
      b = evaluateList xs (snd a)
   in first (appendErrorProne $ fst a) b
  where
    appendErrorProne :: ErrorProne a -> ErrorProne [a] -> ErrorProne [a]
    appendErrorProne ex exs = do
      x <- ex
      xs <- exs
      Right (x : xs)