{-# OPTIONS_GHC -fasm #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Picture where

import Foreign.C.Types

foreign export ccall calcR :: CInt -> CInt -> CUChar
foreign export ccall calcG :: CInt -> CInt -> CUChar
foreign export ccall calcB :: CInt -> CInt -> CUChar

calcWith f = fromIntegral . f . transform theImage . g
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

translate :: Point -> Image a -> Image a
translate (a,b) f = f . (\(x,y) -> (x-a,y-b))

scale :: Point -> Image a -> Image a
scale (a,b) f = f . (\(x,y) -> (x/a,y/b))
