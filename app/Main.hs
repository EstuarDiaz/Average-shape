import Function (eval)
import Fourier (ComplexFunction, inverseFT, averageFS, fourierTransform)

main :: IO ()
main = do
  let xs = [2, 4] :: [ComplexFunction]
  let f = averageShape xs
  print $ eval f 0

type Contour = ComplexFunction

averageShape :: [Contour] -> Contour
averageShape = inverseFT . averageFS . map fourierTransform