module Function (Function(Function), eval, integrate) where

import Data.List (iterate')

newtype Function a b = Function (a -> b)
eval :: Function a b -> a -> b
eval (Function f) = f

instance Functor (Function a) where
  fmap h (Function f) = Function (h . f)

instance Applicative (Function a) where
  pure x = Function (const x)

  (<*>) (Function h) f = Function (\t -> h t (eval f t))

instance Num b => Num (Function a b) where
  (+) (Function f) (Function g) = Function (\t -> f t + g t)
  (-) (Function f) (Function g) = Function (\t -> f t - g t)
  (*) (Function f) (Function g) = Function (\t -> f t * g t)

  abs = fmap abs
  signum = fmap signum

  fromInteger = pure . fromInteger

integrate :: (Fractional a, Real a, Fractional b) => 
  Function a b -> a -> a -> Int -> b
integrate f a b n = 
  let
    dt = (b - a) / fromIntegral n
    interval = take n $ iterate' (+dt) a
  in
    (* realToFrac dt) . sum . map (eval f) $ interval