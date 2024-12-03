{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}

module Fourier where

import Data.Complex (Complex(..), cis, magnitude, phase)
import Data.List (iterate', tails)

import Function (Function(Function), eval, integrate, scale)

type Time   = Float
type Value  = Float
center :: Int
center = 40

type ComplexFunction = Function Time (Complex Value)
type FourierSeries = [Complex Value]

fourierTransform :: ComplexFunction -> FourierSeries
fourierTransform f = 
  let
    exp' :: Int -> ComplexFunction
    exp' k = Function (\t -> cis (- fromIntegral k * t))
    coefficient :: Int -> Complex Value
    coefficient k = integrate (f * exp' k) 0 (2 * pi) 100
  in 
    map coefficient [-center .. center]

inverseFT :: FourierSeries -> ComplexFunction
inverseFT coefficients = 
  Function $
    \t -> (/(2 * pi)). sum $ 
        zipWith
          (\ ck k -> ck * cis (fromIntegral k * t))
          coefficients [- center .. center]


instance Num FourierSeries where
  (+), (-), (*) :: FourierSeries -> FourierSeries -> FourierSeries
  (+) = zipWith (+)
  (-) = zipWith (-)
  (*) hs xs = -- convolution
    let pad = replicate (length hs - 1) 0
        ts  = pad ++ xs
    in map (sum . zipWith (*) (reverse hs)) (init $ tails ts)

  abs, signum :: FourierSeries -> FourierSeries
  abs = map (realToFrac. magnitude)
  signum = map (cis. phase)

  fromInteger :: Integer -> FourierSeries
  fromInteger n = replicate center 0 ++ [fromInteger n] ++ replicate center 0

averageFS :: [FourierSeries] -> FourierSeries
averageFS xs = map (/ fromIntegral (length xs)) $ sum xs

averageShape :: [ComplexFunction] -> ComplexFunction
averageShape = inverseFT . averageFS . map fourierTransform

averageFSWeighted :: [FourierSeries] -> [FourierSeries]
averageFSWeighted (f : rest@(g : _)) = 
  [zipWith (\x y -> c * x + (1 - c) * y) f g | i <- [0, 0.05 .. 1], let c = i :+ 0] 
    ++ averageFSWeighted rest
averageFSWeighted _ = []

averageShapeWeighted :: [ComplexFunction] -> [ComplexFunction]
averageShapeWeighted = map inverseFT . averageFSWeighted . map fourierTransform