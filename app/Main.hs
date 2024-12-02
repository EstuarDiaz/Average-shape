import Function (eval)
import Fourier (averageShape)
import Contour
import Codec.Picture 
import ContourToFunction

main :: IO ()
main = do
  imgs <- mapM contour ["square.png", "circle.png"]
  let averagePerimeter = toContour . averageShape . map toFunction $ imgs
  generateShape (scatter averagePerimeter) "averagePerimeter.png"