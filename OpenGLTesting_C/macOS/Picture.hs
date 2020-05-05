{-# OPTIONS_GHC -fasm #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- Set -fobject-code in GHCi
module Picture where

import Foreign.C.Types
import Control.Applicative
import Data.Bits hiding (rotate)
import Data.Function

--                                  R      G      B
-- newtype Color = Color { unColor :: (Float, Float, Float) } deriving Show
data Color = Color {r,g,b :: {-# UNPACK #-} !Float}
type Point = (Float, Float)

type Image a = Point -> a
type Region = Image Bool
type Filter c = Image c -> Image c
type FilterC = Filter Color
type ImageC = Image Color

renderRegion :: Region -> Image Color
renderRegion r p = if r p then black else white
  
instance Semigroup Color where
  Color a b c <> Color x y z = Color (a + x) (b + y) (c + z)

instance Monoid Color where
  mempty = black

black = Color 0 0 0
white = Color 255 255 255

theImage :: Image Color
theImage (x,y) | x * x + y * y <= 10000 = Color (x + 20) (y + 30) (x + y)
               | otherwise              = mempty

cond :: Image Bool -> Image c -> Image c -> Image c
cond = liftA3 (\a b c -> if a then b else c)

crop :: Region -> FilterC
crop reg im = cond reg im mempty

type Warp = Point -> Point
type Vector = (Float, Float)


translateP :: Vector -> Warp
translateP (dx, dy) (x, y) = (x + dx, y + dy)

scaleP :: Vector -> Warp
scaleP (sx, sy) (x, y) = (sx * x, sy * y)

uscaleP :: Float -> Warp
uscaleP s = scaleP (s,s)

rotateP :: Float -> Warp
rotateP θ (x,y) = (x * cos θ - y * sin θ, y * cos θ + x * sin θ)

udisk :: Region
udisk p = distO p < 1

invWarp warp im = im . warp

translate :: Vector -> Filter c
translate (dx,dy) = invWarp (translateP (-dx, -dy))

scale :: Vector -> Filter c
scale (sx,sy) = invWarp (scaleP (1/sx, 1/sy))

uscale :: Float -> Filter c
uscale s = invWarp (uscaleP (1/s))

rotate :: Float -> Filter c
rotate θ = invWarp (rotateP (-θ))

calculate extract = floor . extract . adjust image . toFloat
  where
    image = renderFrac wavDist
    (screenWidth, screenHeight) = (640, 480)
    toFloat :: (CInt, CInt) -> (Float, Float)
    toFloat (x,y) = (fromIntegral x, fromIntegral y)
    adjust = adjustToWindow
    adjustToWindow :: Filter c
    adjustToWindow = translate (screenWidth / 2, screenHeight / 2)
                   . scale (30,30)
                   . rotate (-pi/2)

foreign export ccall calcR :: CInt -> CInt -> CUChar
foreign export ccall calcG :: CInt -> CInt -> CUChar
foreign export ccall calcB :: CInt -> CInt -> CUChar

calcR :: CInt -> CInt -> CUChar
calcR x y = calculate f (x,y)
  where
    f (Color x _ _) = x

calcG :: CInt -> CInt -> CUChar
calcG x y = calculate f (x,y)
  where
    f (Color _ y _) = y

calcB :: CInt -> CInt -> CUChar
calcB x y = calculate f (x,y)
  where
    f (Color _ _ z) = z

checker :: Region
checker (x,y) = even (floor x + floor y)

vstrip :: Region
vstrip (x,y) = fromIntegral (floor x) <= 1 / 2

overlay :: ImageC -> ImageC -> ImageC
overlay = liftA2 (<>)

gasket :: Region
gasket (x,y) = (floor x) .|. (floor y) == (floor x :: Integer)

altRings p = even (floor (distO p))

distO (x,y) = sqrt (x * x + y * y)

type Frac = Float -- in [0, 1]

wavDist :: Image Frac
wavDist p = (1 + cos (pi * distO p)) / 2

renderFrac :: Image Frac -> ImageC
renderFrac f (x,y) = lerpC (f (x,y)) black white

lerpC :: Frac -> Color -> Color -> Color
lerpC w (Color r1 g1 b1) (Color r2 g2 b2) = Color (h r1 r2) (h g1 g2) (h b1 b2)
  where
    h x1 x2 = w * x1 + (1 - w) * x2

type PolarPoint = (Float, Float)
fromPolar :: Point -> PolarPoint
fromPolar (ρ, θ) = (ρ * cos θ, ρ * sin θ)

toPolar :: PolarPoint -> Point
toPolar (x,y) = (distO (x,y), atan2 y x)

polarChecker :: Int -> Region
polarChecker = polarize checker

polarize :: Image c -> Int -> PolarPoint -> c
polarize pat n = pat . sc . toPolar
  where
    sc (ρ, θ) = (ρ, θ * n' / π)
    π = pi
    n' = fromIntegral n

  
