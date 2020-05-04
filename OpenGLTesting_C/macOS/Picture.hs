{-# OPTIONS_GHC -fasm #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Picture where

import Foreign.C.Types

foreign export ccall calcR :: CInt -> CInt -> CUChar
foreign export ccall calcG :: CInt -> CInt -> CUChar
foreign export ccall calcB :: CInt -> CInt -> CUChar

g :: (CInt, CInt) -> (Int, Int)
g (x,y) = (fromIntegral x, fromIntegral y)
calcR :: CInt -> CInt -> CUChar
calcR x y = fromIntegral $ f $ theImage $ g (x,y)
  where f (Color (x,y,z)) = x

calcG :: CInt -> CInt -> CUChar
calcG x y = fromIntegral $ f $ theImage $ g (x,y)
  where f (Color (x,y,z)) = y

calcB :: CInt -> CInt -> CUChar
calcB x y = fromIntegral $ f $ theImage $ g (x,y)
  where f (Color (x,y,z)) = z

--            R    G    B
newtype Color = Color { unColor :: (Int, Int, Int) } deriving Show
-- instance 
type Point = (Int, Int)

type Image = Point -> Color

instance Semigroup Color where
  Color (a,b,c) <> Color (x,y,z) = Color (a + x, b + y, c + z)

instance Monoid Color where
  mempty = black

black = Color (0,0,0)
white = Color (255,255,255)

theImage :: Image
theImage (x,y) | x * x + y * y <= 10000 = white
               | otherwise              = mempty
