{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fasm #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Picture where

import Foreign.C.Types
import Control.Applicative
import Data.Bits
--                                  R      G      B
newtype Color = Color { unColor :: (Float, Float, Float) } deriving Show

type Point = (Float, Float)

type Image a = Point -> a
type Region = Image Bool
type Filter c = Image c -> Image c
type FilterC = Filter Color
type ImageC = Image Color

regionToImage :: Region -> Image Color
regionToImage r p = if r p then black else white
  
instance Semigroup Color where
  Color (a,b,c) <> Color (x,y,z) = Color (a + x, b + y, c + z)

instance Monoid Color where
  mempty = black

black = Color (0,0,0)
white = Color (255,255,255)

theImage :: Image Color
theImage (x,y) | x * x + y * y <= 10000 = Color (x + 20, y + 30, x + y)
               | otherwise              = mempty

cond :: Image Bool -> Image c -> Image c -> Image c
cond = liftA3 (\a b c -> if a then b else c)

translate :: Point -> Filter c
-- translate :: Point -> Image c -> Image c
translate (a,b) f (x,y) = f (x-a,y-b)

scale :: Point -> Filter c
scale (a,b) f (x,y) = f (x/a,y/b)

crop :: Region -> FilterC
crop reg im = cond reg im mempty

foreign export ccall calcR :: CInt -> CInt -> CUChar
foreign export ccall calcG :: CInt -> CInt -> CUChar
foreign export ccall calcB :: CInt -> CInt -> CUChar

-- calcWith :: (Color -> Float) -> (CInt, CInt) -> CUChar
-- calcWith f = floor . f . crop (scale (10,10) checker) image' . g
--   where
--     g :: (CInt, CInt) -> (Float, Float)
--     g (x,y) = (fromIntegral x, fromIntegral y)
--     image' = (scale (2,2) . translate (100,100)) theImage
    -- transform :: Region -> Image Color
    -- transform = regionToImage . scale (100,100) . translate (100, 100)

calcWith f = floor . f . transform gasket . g
  where
    g :: (CInt, CInt) -> (Float, Float)
    g (x,y) = (fromIntegral x, fromIntegral y)
    transform = regionToImage
calcR :: CInt -> CInt -> CUChar
calcR x y = calcWith f (x,y)
  where
    f (Color (x,_,_)) = x

calcG :: CInt -> CInt -> CUChar
calcG x y = calcWith f (x,y)
  where
    f (Color (_,y,_)) = y

calcB :: CInt -> CInt -> CUChar
calcB x y = calcWith f (x,y)
  where
    f (Color (_,_,z)) = z

checker :: Region
checker (x,y) = even (floor x + floor y)

newtype Image' a = Image' (Point -> a)

instance Functor Image' where
  fmap :: (a -> b) -> Image' a -> Image' b
  fmap f (Image' i) = Image' (f . i)

-- instance Applicative Image' where


-- Endofunctor in Haskell
-- [1] fmap (f . g) x = (fmap f . fmap g) x
-- [2] fmap id      x = x

-- Proof of [1]
--   fmap (f . g) (Image' i)
-- = { unfold fmap }
--   Image' ((f . g) . i)
-- = { composition is associative }
--   Image' (f . (g . i))
-- = { fold fmap }
--   fmap f (Image' (g . i))
-- = { fold fmap }
--   fmap f (fmap g (Image' i))
-- = { fold (.) }
--   (fmap f . fmap g) (Image' i)
-- []

-- Proof of [2]
--   fmap id (Image' i)
-- = { unfold fmap }
--   Image' (id . i)
-- = { compose id }
--   Image' i
-- []

instance Applicative Image' where
  pure :: a -> Image' a
  pure x = Image' (const x)
  liftA2 :: (a -> b -> c) -> Image' a -> Image' b -> Image' c
  liftA2 f (Image' a) (Image' b) = Image' (\p -> f (a p) (b p))

liftA2' :: (a -> b -> c) -> Image a -> Image b -> Image c
liftA2' f a b = (\p -> f (a p) (b p))

overlay :: ImageC -> ImageC -> ImageC
overlay = liftA2' (<>)

gasket :: Region
gasket (x,y) = (floor x) .|. (floor y) == (floor x :: Integer)
