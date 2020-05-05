{-# OPTIONS_GHC -fasm #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Picture where

import Foreign.C.Types
import Control.Applicative

foreign export ccall calcR :: CInt -> CInt -> CUChar
foreign export ccall calcG :: CInt -> CInt -> CUChar
foreign export ccall calcB :: CInt -> CInt -> CUChar

calcWith f = fromIntegral . f . transform (crop (scale (10, 10) checker) theImage) . g
  where
    g (x,y) = (fromIntegral x, fromIntegral y)
    transform = scale (2,2) . translate (100, 100)

calcR :: CInt -> CInt -> CUChar
calcR x y = calcWith f (x,y)
  where
    f (Color (x,_,_)) = floor x

calcG :: CInt -> CInt -> CUChar
calcG x y = calcWith f (x,y)
  where
    f (Color (_,y,_)) = floor y

calcB :: CInt -> CInt -> CUChar
calcB x y = calcWith f (x,y)
  where
    f (Color (_,_,z)) = floor z

--                                  R    G    B
newtype Color = Color { unColor :: (Float, Float, Float) } deriving Show

type Point = (Float, Float)

type Image a = Point -> a
type Region = Image Bool
type Filter c = Image c -> Image c
type FilterC = Filter Color

regionToImage :: Region -> Image Color
regionToImage f = (\b -> if b then white else black) . f
  
instance Semigroup Color where
  Color (a,b,c) <> Color (x,y,z) = Color (a + x, b + y, c + z)

instance Monoid Color where
  mempty = black

black = Color (0,0,0)
white = Color (255,255,255)

theImage :: Image Color
theImage (x,y) | x * x + y * y <= 10000 = Color (x + 20, y + 30, x + y)
               | otherwise              = black

cond :: Image Bool -> Image c -> Image c -> Image c
cond = liftA3 (\a b c -> if a then b else c)
translate :: Point -> Filter c
translate (a,b) f = f . (\(x,y) -> (x-a,y-b))

scale :: Point -> Filter c
scale (a,b) f = f . (\(x,y) -> (x/a,y/b))

crop :: Region -> FilterC
crop reg im = cond reg im mempty

checker :: Region
checker (x,y) = (even (floor x + floor y)) --(even (floor x)) || (even (floor y))