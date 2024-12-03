import Function (eval)
import Fourier (averageShape)
import Contour
import Codec.Picture 
import ContourToFunction

main :: IO ()
main = do
  perimeter <- contour "italy.png"
  generateShape 350 350 (scatter perimeter) "perimeterItaly.png"
  let perimeter' = toContour . toFunction $ perimeter
  generateShape 350 350 (scatter perimeter') "italyPlot.png"

  perimeter <- contour "germany.png"
  generateShape 350 350 (scatter perimeter) "perimeterGermany.png"
  let perimeter' = toContour . toFunction $ perimeter
  generateShape 350 350 (scatter perimeter') "germanyPlot.png"
  
  --
  
  imgs <- mapM contour ["italy.png", "germany.png"]
  let averagePerimeter = toContour . averageShape . map toFunction $ imgs
  generateShape 350 350 (scatter averagePerimeter) "averagePerimeter.png"