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
intFuncSingle f = fmap (fromIntegral . f) . toIntegral

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
aDefault x = (Right x,)

aError :: String -> StdGen -> (ErrorProne Double, StdGen)
aError s = (Left s,)

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
applyOperator _ [] = aError "too few operands"
applyOperator o [a, b, c]
  | o == 't' = applyDiceRemove (toIntegral a) (toIntegral b) (toIntegral c) True
  | o == 'b' = applyDiceRemove (toIntegral a) (toIntegral b) (toIntegral c) False
applyOperator o [a, b]
  | o == '+' = aDefault (a + b)
  | o == '-' = aDefault (a - b)
  | o == '*' = aDefault (a * b)
  | o == '/' = aDefault (a / b)
  | o == '^' = aDefault (a ** b)
  | o == '%' = (errorMessege (intFuncDouble' mod' a b) "mod input must be integers with a nonzero dividend",)
  | o == 'd' = applyDice (toIntegral a) (toIntegral b)
  | otherwise = aError "unknown operator"
applyOperator o [a]
  | o == '+' = aDefault a
  | o == '-' = aDefault (-a)
  | o == '~' = aDefault (-a)
  | o == 'd' = applyDice (Just 1) (toIntegral a)
  | o == '%' = applyDice (toIntegral a) (Just 100)
  | o == '!' = (errorMessege (intFuncSingle' fac a) "factorial input must be a positive integer",)
applyOperator _ xs = aError "too many operands"

applyFunction :: String -> [Double] -> StdGen -> (ErrorProne Double, StdGen)
applyFunction o []
  | o == "pi" = aDefault pi
  | o == "tau" = aDefault (pi * 2)
  | o == "e" = aDefault (exp 1)
  | otherwise = aError "unknown constant"
applyFunction o [x]
  | o == "negate" = aDefault (negate x)
  | o == "abs" = aDefault (abs x)
  | o == "sign" = aDefault (signum x)
  | o == "round" = aDefault (fromIntegral $ round x)
  | o == "trunc" = aDefault (fromIntegral $ truncate x)
  | o == "floor" = aDefault (fromIntegral $ floor x)
  | o == "ceil" = aDefault (fromIntegral $ ceiling x)
  | o == "inv" = aDefault (x ** (-1))
  | o == "exp" = aDefault (exp x)
  | o == "sqrt" = aDefault (sqrt x)
  | o == "ln" = aDefault (log x)
  | o == "log" = aDefault (logBase 10 x)
  | o == "fac" = (errorMessege (intFuncSingle' fac x) "factorial input must be a positive integer",)
  | o == "sin" = aDefault (sin x)
  | o == "tan" = aDefault (tan x)
  | o == "cos" = aDefault (cos x)
  | o == "asin" = aDefault (asin x)
  | o == "atan" = aDefault (atan x)
  | o == "acos" = aDefault (acos x)
  | o == "rad" = aDefault (x / 180 * pi)
  | o == "deg" = aDefault (x / pi * 180)
applyFunction o [a, b]
  | o == "add" = aDefault (a + b)
  | o == "sub" = aDefault (a - b)
  | o == "mult" = aDefault (a * b)
  | o == "div" = aDefault (a / b)
  | o == "mod" = (errorMessege (intFuncDouble' mod' a b) "mod input must be integers with a nonzero dividend",)
  | o == "rem" = (errorMessege (intFuncDouble' rem' a b) "mod input must be integers with a nonzero dividend",)
  | o == "pow" = aDefault (a ** b)
  | o == "log" = aDefault (logBase b a)
applyFunction o l@(x : xs)
  | o == "sum" = aDefault (sum l)
  | o == "prod" = aDefault (product l)
  | o == "min" = aDefault (minimum l)
  | o == "max" = aDefault (maximum l)
  | otherwise = aError "unknown function"

-- evaluate :: ErrorProne (Tree Token) -> StdGen -> ErrorProne Double
-- evaluate t g = fst <$> evaluate' t g

evaluate :: ErrorProne (Tree Token) -> StdGen -> (ErrorProne Double, StdGen)
evaluate (Left e) = (Left e,)
evaluate (Right (Leaf x)) = case x of
  Number n -> (Right n,)
  _ -> aError "bad input"
evaluate (Right (Branch x l)) = case x of
  Operator c -> uncurry (applyList (applyOperator c)) . evaluateList l
  Function s -> uncurry (applyList (applyFunction s)) . evaluateList l
  _ -> aError "bad input"
  where
    applyList :: ([Double] -> StdGen -> (ErrorProne Double, StdGen)) -> ErrorProne [Double] -> StdGen -> (ErrorProne Double, StdGen)
    applyList f (Left e) = (Left e,)
    applyList f (Right xs) = f xs

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