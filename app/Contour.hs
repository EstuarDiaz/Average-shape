module Contour where

-- install Haskell Image Processing library using `$ cabal install hip`

import Codec.Picture

imageCreator :: String -> IO ()
imageCreator path = writePng path $ generateImage pixelRenderer 250 300
   where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128:qTyConKey