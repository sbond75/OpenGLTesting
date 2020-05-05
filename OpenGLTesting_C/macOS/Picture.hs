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
-- ^^^This is like making a struct with an int inside it or something -- this means we can't just treat it as an int.

type Point = (Float, Float)

type Image a = Point -> a
type Region = Image Bool
type Filter c = Image c -> Image c
type FilterC = Filter Color
type ImageC = Image Color -- Same as Point -> Color

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

-- Make Image' just to show how functor works because Haskell already
-- implements Functor, Applicative, etc. FOR YOU and doing this for "Image"
-- and not "Image'" would cause an error.
newtype Image' a = MkImage' (Point -> a)
instance Functor Image' where
  fmap :: (a -> b) -> Image' a -> Image' b
  fmap f (MKImage' i) = MkImage' (f . i) --(\p -> f (i p))
  -- (f . g) = \x -> f (g x) where x is the point.

newtype Person = Person {-<-- The tag-} String
getName (Person name) = name
foo (Person name) = Person (name ++ name)

--instance Applicative Image' where

-- Endofunctor in Haskell:
-- A functor between two categories C and D such that
-- when I do F (f . g) = F f . F g               ("." is compose)
-- F idC               = idD

-- in haskell it is: fmap (f . g) = fmap f . fmap g
-- fmap id                        = id

-- fmap (f . g)

-- Applicative has two methods:
-- pure
-- liftA2

instance Applicative Image' where
  pure :: a -> Image' a
  pure x = MkImage' (\p -> x)
  liftA2 :: (a -> b -> c) -> Image' a -> Image' b -> Image' c
  liftA2 f (MkImage' a) (MkImage' b) -> MkImage' (\p -> (f (a p) (b p)))

liftA2' :: (a -> b -> c) -> Image a -> Image b -> Image c
liftA2' f a b = (\p -> f (a p) (b p))

overlay :: ImageC -> ImageC -> ImageC
overlay = liftA2' addColor
  where 
    --this wouldn't work because Color is an opaque type...: addColor :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float)
    addColor :: ImageC -> ImageC -> ImageC
    addColor (Color (a,b,c)) (Color (x,y,z)) = Color (a + x, b + y, c + z)

-- From Haskell standard library:
--instance Functor ((->) r) where
  -- ...