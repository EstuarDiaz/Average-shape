module Contour where

-- install Haskell Image Processing library using `$ cabal install hip`
import Data.Maybe (fromMaybe)
import Data.List (find, elem)
import Data.Either (fromRight)
import Codec.Picture

max_iterations :: Int
max_iterations = 2000
precission :: Int
precission = 1

center :: (Int, Int)
center = (206, 180)
color, bg_color, renderColor :: PixelRGB8
color = PixelRGB8 0 0 0
bg_color = PixelRGB8 255 255 255
renderColor = PixelRGB8 0 0 0

type Contour = [(Int, Int)]

type PixelMask = Int -> Int -> Bool

isInside :: PixelRGB8 -> Bool
isInside (PixelRGB8 r g b) = r + g + b < 50

isOutside :: PixelRGB8 -> Bool
isOutside = not . isInside

generateShape :: Int -> Int -> PixelMask -> String -> IO ()
generateShape w h mask path = writePng path $ generateImage pixelRenderer w h
   where pixelRenderer x y = if mask x y then bg_color else renderColor

imageCreator :: Int -> Int -> PixelMask -> Image PixelRGB8
imageCreator w h mask = generateImage pixelRenderer w h
   where pixelRenderer x y = if mask x y then bg_color else renderColor

square :: Int -> PixelMask
square r x y = 
   let 
      (x', _) = center
      a = x' - r
      b = x' + r
   in a <= x && x <= b && a <= y && y <= b

circle :: Int -> PixelMask
circle r x y = let (x', y') = center in (x - x')^2 + (y - y')^2 < r^2

scatter :: [(Int, Int)] -> PixelMask
scatter xs x y = (x,y) `elem` xs

isCloseTo :: (Int, Int) -> (Int, Int) -> Bool
isCloseTo (x,y) (z,w) = abs (x - z) + abs (y - w) < precission

findContour :: Image PixelRGB8 -> PixelRGB8 -> PixelRGB8 -> (Int, Int) -> [(Int, Int)]
findContour img color bg_color point = findContour' 1 starting_point
   where 
      starting_point = findFirstEdge point

      findFirstEdge :: (Int, Int) -> (Int, Int)
      findFirstEdge (x, y) = if isOutside $ pixelAt img (x+1) y
                           then (x, y)
                           else findFirstEdge (x + 1, y)

      findContour' :: Int -> (Int, Int) -> [(Int, Int)]
      findContour' iterations current_point
         | iterations >= max_iterations = []
         | otherwise =
            let next_point = findNextPointClockwise current_point
            in if current_point `isCloseTo` starting_point && iterations > precission 
               then [] 
               else current_point : findContour' (iterations + 1) next_point

      findNextPointClockwise :: (Int, Int) -> (Int, Int)
      findNextPointClockwise (x,y) =
         let
            d = precission -- maskSize
            nbhd = reverse $     
                        [(x + i, y + d) | i <- [-d .. d]] 
                     ++ [(x + d, y + i) | i <- [d, d - 1 .. -d]]  
                     ++ [(x + i, y - d) | i <- [d, d - 1 .. - d]]
                     ++ [(x - d, y + i) | i <- [-d .. d]]

            edgeIn (a,b) (w,z) = (isInside $ pixelAt img a b) && (isOutside $ pixelAt img w z)
            next_pixel = fst <$> find (uncurry edgeIn)  (zip (cycle $ drop 1 nbhd) nbhd)
         in
            fromMaybe (x,y) next_pixel

contour :: String -> IO (Contour)
contour filePath = do
  x <- readImage filePath
  let img = convertRGB8 . fromRight undefined $ x
  let img_center = (imageWidth img `div` 2, imageHeight img `div` 2)
  return $ findContour img color bg_color img_center