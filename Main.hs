import Fourier

main :: IO ()
main = do
  print "Hello"

type Contour = ComplexFunction

averageShape :: [Contour] -> Contour
averageShape = inverseFT . average . map fourierTransform