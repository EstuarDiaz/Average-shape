import Function (eval)
import Fourier (ComplexFunction, inverseFT, averageFS, fourierTransform)
import Contour
import Codec.Picture 

main :: IO ()
main = do
  --let xs = [2, 4] :: [ComplexFunction]
  --let f = averageShape xs
  --print $ eval f 0
  --imageCreator "test.png"
  generateShape (square 25) "square.png"
  generateShape (circle 25) "circle.png"
  perimeter <- contour "circle.png"
  print perimeter
  generateShape (scatter perimeter) "perimeter.png"

averageShape :: [ComplexFunction] -> ComplexFunction
averageShape = inverseFT . averageFS . map fourierTransform