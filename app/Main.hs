import Data.Either (fromRight)

import Function (eval)
import Fourier (averageShapeWeighted)
import Contour
import Codec.Picture 
import ContourToFunction

main :: IO ()
main = do
  imgs <- mapM contour ["italy.png", "germany.png", "italy.png"]
  let transition =  map (imageCreator 350 350 . scatter . toContour)
                    . averageShapeWeighted . map toFunction $ imgs
  fromRight undefined  $ writeGifAnimation "transition.gif" 1 LoopingForever transition