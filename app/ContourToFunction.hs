module ContourToFunction where

import Data.Complex (Complex(..),realPart, imagPart)

import Contour (Contour, center)
import Function (Function(..), eval)
import Fourier (Time, Value, ComplexFunction)

epsilon :: Float
epsilon = 0.1

toFunction :: Contour -> ComplexFunction
toFunction xs = Function f
  where
    (x', y') = center
    len = length xs
    index :: Time -> Int
    index t = round (t * (fromIntegral len) / (2 * pi)) `rem` len
    f :: Time -> Complex Value
    f t = let (a, b) = xs !! (index t) in (fromIntegral (a - x')) :+ (fromIntegral (b - y'))

toContour :: ComplexFunction -> Contour
toContour f =
  let
    (x', y') = center
    toPair z = (x' + (round $ realPart z), y' + (round $ imagPart z))
  in 
    map (toPair . eval f) [0, epsilon .. 2 * pi :: Float]
