module Test where

import Function (eval)
import Fourier (ComplexFunction, inverseFT, averageFS, fourierTransform)
import Contour
import Codec.Picture 
import ContourToFunction

testComplexFunction :: IO ()
testComplexFunction = do
  let xs = [2, 4] :: [ComplexFunction]
  let f = averageShape xs
  print $ eval f 0

testImageGenerator :: IO ()
testImageGenerator = do
  imageCreator "test.png"
  generateShape (square 25) "square.png"
  generateShape (circle 25) "circle.png"
  
testPerimeter :: IO ()
testPerimeter = do
  perimeter <- contour "circle.png"
  print perimeter
  generateShape (scatter perimeter) "perimeter.png"
  let perimeter' = toContour . toFunction $ perimeter
  generateShape (scatter perimeter') "perimeter2.png"

testAveragePerimeter :: IO ()
testAveragePerimeter = do
  perimeter <- contour "circle.png"
  generateShape (scatter perimeter) "circlePerimeter.png"
  
  perimeter' <- contour "square.png"
  generateShape (scatter perimeter') "squarePerimeter.png"