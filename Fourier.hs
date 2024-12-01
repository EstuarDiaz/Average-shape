module Fourier where

import Data.Complex (Complex(..), magnitude, cis, mkPolar)
import Data.List (iterate')

type Time = Float

newtype Function a = Function (Time -> a)
eval :: Function a -> Time -> a
eval (Function f) = f

instance Functor Function where
  fmap :: (a -> b) -> Function a -> Function b
  fmap h (Function f) = Function (h . f)

instance Applicative Function where
  pure :: a -> Function a
  pure x = Function (const x)

  (<*>) :: Function (a -> b) -> Function a -> Function b
  (<*>) (Function h) f = Function (\t -> h t (eval f t))

instance Num a => Num (Function a) where
  (+),(-),(*) :: Function a -> Function a -> Function a
  (+) (Function f) (Function g) = Function (\t -> f t + g t)
  (-) (Function f) (Function g) = Function (\t -> f t - g t)
  (*) (Function f) (Function g) = Function (\t -> f t * g t)

  abs, signum :: Function a -> Function a
  abs = fmap abs
  signum = fmap signum

  fromInteger :: Integer -> Function a
  fromInteger = pure . fromInteger

type Value = Float
center :: Int
center = 10
i :: Complex Value
i = 0 :+ 1

type ComplexFunction = Function (Complex Value)
type FourierSeries = [Complex Value]

integrate :: ComplexFunction -> Time -> Time -> Complex Value
integrate f a b = 
  let
    n = 100 :: Int
    dt = (b - a) / (fromIntegral n - 1)
  in 
    sum . map (eval f) . take n $ iterate' (+dt) a

fourierTransform :: ComplexFunction -> FourierSeries
fourierTransform f = 
  let
    exp' :: Int -> ComplexFunction
    exp' k = Function (\t -> cis (- fromIntegral k * t))
    coefficient :: Int -> Complex Value
    coefficient k = integrate (f * exp' k) 0 (2 * pi)
  in 
    map coefficient [-center .. center]

inverseFT :: FourierSeries -> ComplexFunction
inverseFT coefficients = Function $
  \t -> sum $ zipWith
            (\ ck k -> ck * cis (fromIntegral k * t))
            coefficients [- center .. center]


instance Num FourierSeries where
  (+), (-), (*) :: FourierSeries -> FourierSeries -> FourierSeries
  (+) = zipWith (+)
  (-) = zipWith (-)
  (*) = undefined

  abs, signum :: FourierSeries -> FourierSeries
  abs = undefined
  signum = undefined

  fromInteger :: Integer -> FourierSeries
  fromInteger n = replicate center 0 ++ [fromInteger n] ++ replicate center 0

average :: Foldable t => t FourierSeries -> FourierSeries
average xs = map (/ fromIntegral (length xs)) $ sum xs
