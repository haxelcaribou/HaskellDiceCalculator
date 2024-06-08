{-# LANGUAGE TupleSections #-}
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

aDefault :: StdGen -> Double -> ErrorProne (Double, StdGen)
aDefault g x = Right (x, g)

applyDice :: StdGen -> Maybe Int -> Maybe Int -> ErrorProne (Double, StdGen)
applyDice g Nothing _ = Left "dice input must be an integer"
applyDice g _ Nothing = Left "dice input must be an integer"
applyDice g (Just num) (Just sides) = first fromIntegral <$> Dice.rollDice g num sides

applyOperator :: StdGen -> Char -> [Double] -> ErrorProne (Double, StdGen)
applyOperator g _ [] = Left "too few operands"
applyOperator g _ (a : b : c : xs) = Left "too many operands"
applyOperator g o [a, b]
  | o == '+' = aDefault g $ a + b
  | o == '-' = aDefault g $ a - b
  | o == '*' = aDefault g $ a * b
  | o == '/' = aDefault g $ a / b
  | o == '^' = aDefault g $ a ** b
  | o == '%' = (, g) <$> errorMessege (intFuncDouble' mod' a b) "mod input must be an integer with a nonzero dividend"
  | o == 'd' = applyDice g (toIntegral a) (toIntegral b)
  | otherwise = Left "unknown operator"
applyOperator g o [a]
  | o == '+' = aDefault g a
  | o == '-' = aDefault g $ -a
  | o == '~' = aDefault g $ -a
  | o == 'd' = applyDice g (Just 1) (toIntegral a)
  | o == '!' = (, g) <$> errorMessege (intFuncSingle' fac a) "factorial input must be a positive integer"

applyFunction :: StdGen -> String -> [Double] -> ErrorProne (Double, StdGen)
applyFunction g o []
  | o == "pi" = aDefault g pi
  | o == "e" = aDefault g $ exp 1
  | otherwise = Left "unknown constant"
applyFunction g o [x]
  | o == "negate" = aDefault g $ negate x
  | o == "abs" = aDefault g $ abs x
  | o == "sign" = aDefault g $ signum x
  | o == "round" = aDefault g $ fromIntegral $ round x
  | o == "trunc" = aDefault g $ fromIntegral $ truncate x
  | o == "floor" = aDefault g $ fromIntegral $ floor x
  | o == "ceil" = aDefault g $ fromIntegral $ ceiling x
  | o == "exp" = aDefault g $ exp x
  | o == "sqrt" = aDefault g $ sqrt x
  | o == "ln" = aDefault g $ log x
  | o == "log" = aDefault g $ logBase 10 x
  | o == "fac" = (, g) <$> errorMessege (intFuncSingle' fac x) "factorial input must be a positive integer"
  | o == "sin" = aDefault g $ sin x
  | o == "tan" = aDefault g $ tan x
  | o == "cos" = aDefault g $ cos x
  | o == "asin" = aDefault g $ asin x
  | o == "atan" = aDefault g $ atan x
  | o == "acos" = aDefault g $ acos x
  | o == "rad" = aDefault g $ x / 180 * pi
  | o == "deg" = aDefault g $ x / pi * 180
applyFunction g o [a, b]
  | o == "add" = aDefault g $ a + b
  | o == "sub" = aDefault g $ a - b
  | o == "mult" = aDefault g $ a * b
  | o == "div" = aDefault g $ a / b
  | o == "mod" = (, g) <$> errorMessege (intFuncDouble' mod' a b) "mod input must be an integer with a nonzero dividend"
  | o == "rem" = (, g) <$> errorMessege (intFuncDouble' rem' a b) "mod input must be an integer with a nonzero dividend"
  | o == "pow" = aDefault g $ a ** b
  | o == "log" = aDefault g $ logBase b a
applyFunction g o l@(x : xs)
  | o == "sum" = aDefault g $ sum l
  | o == "prod" = aDefault g $ product l
  | o == "min" = aDefault g $ minimum l
  | o == "max" = aDefault g $ maximum l
  | otherwise = Left "unknown function"

evaluate :: StdGen -> ErrorProne (Tree Token) -> ErrorProne Double
evaluate g t = fst <$> evaluate' g t

evaluate' :: StdGen -> ErrorProne (Tree Token) -> ErrorProne (Double, StdGen)
evaluate' _ (Left e) = Left e
evaluate' g (Right (Leaf x)) = case x of
  Number n -> Right (n, g)
  _ -> Left "bad input"
evaluate' g (Right (Branch x l)) = case x of
  Operator c -> evaluateList g l >>= (\a -> applyOperator (snd a) c (fst a))
  Function s -> evaluateList g l >>= (\a -> applyFunction (snd a) s (fst a))
  _ -> Left "bad input"

evaluateList :: StdGen -> [Tree Token] -> ErrorProne ([Double], StdGen)
evaluateList g [] = Right ([], g)
evaluateList g l@(x : xs) = do
  a <- evaluate' g (Right x)
  b <- evaluateList (snd a) xs
  Right (first (fst a :) b)