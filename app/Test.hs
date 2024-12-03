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
  generateShape 100 100 (square 25) "square.png"
  generateShape 100 100 (circle 25) "circle.png"
  
testPerimeter :: IO ()
testPerimeter = do
  perimeter <- contour "circle.png"
  print perimeter
  generateShape 100 100 (scatter perimeter) "perimeter.png"
  let perimeter' = toContour . toFunction $ perimeter
  generateShape 100 100 (scatter perimeter') "perimeter2.png"

testAveragePerimeter :: IO ()
testAveragePerimeter = do
  imgs <- mapM contour ["square.png", "circle.png"]
  let averagePerimeter = toContour . averageShape . map toFunction $ imgs
  generateShape 100 100 (scatter averagePerimeter) "averagePerimeter.png"

testAverageContour :: IO ()
testAverageContour = do
  imgs <- mapM contour ["italy.png", "germany.png"]
  let averagePerimeter = toContour . averageShape . map toFunction $ imgs
  generateShape 350 350 (scatter averagePerimeter) "averagePerimeter.png"