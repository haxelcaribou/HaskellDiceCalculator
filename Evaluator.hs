{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Evaluator (evaluate) where

import Dice
import Error
import System.Random
import Token
import Tree
import Data.Bifunctor (first, second)

errorMessege :: Maybe a -> String -> ErrorProne a
errorMessege Nothing e = Left e
errorMessege (Just x) _ = Right x

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

applyDefault :: StdGen -> Double -> ErrorProne (Double, StdGen)
applyDefault g x = Right (x, g)

applyDice :: StdGen -> Maybe Int -> Maybe Int -> ErrorProne (Double, StdGen)
applyDice g Nothing _ = Left "dice error"
applyDice g _ Nothing = Left "dice error"
applyDice g (Just num) (Just sides) = errorMessege (first fromIntegral <$> Dice.rollDice g num sides) "dice error"

applyOperator :: StdGen -> Char -> [Double] -> ErrorProne (Double, StdGen)
applyOperator gen _ [] = Left "too few operands"
applyOperator gen _ (a : b : c : xs) = Left "too many operands"
applyOperator gen o [a, b]
  | o == '+' = applyDefault gen $ a + b
  | o == '-' = applyDefault gen $ a - b
  | o == '*' = applyDefault gen $ a * b
  | o == '/' = applyDefault gen $ a / b
  | o == '^' = applyDefault gen $ a ** b
  | o == '%' = (\x -> (x, gen)) <$> errorMessege (intFuncDouble' mod' a b) "mod error"
  | o == 'd' = applyDice gen (toIntegral a) (toIntegral b)
  | otherwise = Left "unknown operator"
applyOperator gen o [a]
  | o == '+' = applyDefault gen a
  | o == '-' = applyDefault gen $ -a
  | o == '~' = applyDefault gen $ -a
  | o == '!' = (\x -> (x, gen)) <$> errorMessege (intFuncSingle' fac a) "factorial error"

applyFunction :: StdGen -> String -> [Double] -> ErrorProne (Double, StdGen)
applyFunction gen o []
  | o == "pi" = applyDefault gen pi
  | o == "e" = applyDefault gen $ exp 1
  | otherwise = Left "unknown constant"
applyFunction gen o [x]
  | o == "negate" = applyDefault gen $ negate x
  | o == "abs" = applyDefault gen $ abs x
  | o == "sign" = applyDefault gen $ signum x
  | o == "round" = applyDefault gen $ fromIntegral $ round x
  | o == "trunc" = applyDefault gen $ fromIntegral $ truncate x
  | o == "floor" = applyDefault gen $ fromIntegral $ floor x
  | o == "ceil" = applyDefault gen $ fromIntegral $ ceiling x
  | o == "exp" = applyDefault gen $ exp x
  | o == "sqrt" = applyDefault gen $ sqrt x
  | o == "ln" = applyDefault gen $ log x
  | o == "log" = applyDefault gen $ logBase 10 x
  | o == "fac" = (\x -> (x, gen)) <$> errorMessege (intFuncSingle' fac x) "factorial error"
  | o == "sin" = applyDefault gen $ sin x
  | o == "tan" = applyDefault gen $ tan x
  | o == "cos" = applyDefault gen $ cos x
  | o == "asin" = applyDefault gen $ asin x
  | o == "atan" = applyDefault gen $ atan x
  | o == "acos" = applyDefault gen $ acos x
  | o == "rad" = applyDefault gen $ x / 180 * pi
  | o == "deg" = applyDefault gen $ x / pi * 180
applyFunction gen o [a, b]
  | o == "add" = applyDefault gen $ a + b
  | o == "sub" = applyDefault gen $ a - b
  | o == "mult" = applyDefault gen $ a * b
  | o == "div" = applyDefault gen $ a / b
  | o == "mod" = (\x -> (x, gen)) <$> errorMessege (intFuncDouble' mod' a b) "mod error"
  | o == "rem" = (\x -> (x, gen)) <$> errorMessege (intFuncDouble' rem' a b) "mod error"
  | o == "pow" = applyDefault gen $ a ** b
  | o == "log" = applyDefault gen $ logBase b a
applyFunction gen o l@(x : xs)
  | o == "sum" = applyDefault gen $ sum l
  | o == "prod" = applyDefault gen $ product l
  | o == "min" = applyDefault gen $ minimum l
  | o == "max" = applyDefault gen $ maximum l
  | otherwise = Left "unknown function"

evaluate :: StdGen -> ErrorProne (Tree Token) -> ErrorProne Double
evaluate g t = fst <$> evaluate' g t

evaluate' :: StdGen -> ErrorProne (Tree Token) -> ErrorProne (Double, StdGen)
evaluate' _ (Left e) = Left e
evaluate' gen (Right (Leaf x)) = case x of
  Number n -> Right (n, gen)
  _ -> Left "bad input"
evaluate' gen (Right (Branch x l)) = case x of
  Operator c -> evaluateList gen l >>= (\a -> applyOperator (snd a) c (fst a))
  Function s -> evaluateList gen l >>= (\a -> applyFunction (snd a) s (fst a))
  _ -> Left "bad input"

evaluateList :: StdGen -> [Tree Token] -> ErrorProne ([Double], StdGen)
evaluateList g [] = Right ([], g)
evaluateList g l@(x : xs) = do
  a <- evaluate' g (Right x)
  b <- evaluateList (snd a) xs
  Right (first (fst a :) b)