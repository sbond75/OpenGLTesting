{-# OPTIONS_GHC -fasm #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Set -fobject-code in GHCi
module Picture where
import           Foreign             hiding (rotate)
import           Foreign.C.Types
import           Foreign.Storable

import           Control.Applicative
import           Control.Monad
import           Data.Bits           hiding (rotate)
import           Data.Function

data Color = Color {r,g,b :: {-# UNPACK #-} !Float}
data CColor = CColor { rC, gC, bC :: {-# UNPACK #-} !CUChar}

-- When we figure out how to interop C structs
-- foreign import ccall  "color.h"
--    newColor :: CUChar -> CUChar -> CUChar -> IO (Ptr CColor)

instance Storable CColor where
       alignment _ = 1
       sizeOf _    = 3
       peek ptr    = CColor
           <$> peekByteOff ptr 0
           <*> peekByteOff ptr 1
           <*> peekByteOff ptr 2
       poke ptr (CColor d c i) = do
           pokeByteOff ptr 0 d
           pokeByteOff ptr 1 c
           pokeByteOff ptr 2 i

-- newtype Colour = Colour { unColou :: (Float, Float, Float
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

black  = Color 0 0 0
white  = Color 1 1 1
red    = Color 1 0 0
green  = Color 0 1 0
blue   = Color 0 0 1
yellow = Color 1 1 0

cond :: Image Bool -> Image c -> Image c -> Image c
cond = liftA3 (\a b c -> if a then b else c)

lerpI :: Image Frac -> ImageC -> ImageC -> ImageC
lerpI = liftA3 lerpC

empty = const mempty
whiteI = const white
blackI = const black
redI = const red
blueI = const blue
greenI = const green
yellowI = const yellow

blackWhiteIm reg = cond reg blackI whiteI
blueYellowIm reg = cond reg blueI yellowI

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

swirlP :: Float -> Warp
swirlP r p = rotateP (distO p * (2 * pi / r)) p

swirl :: Float -> Filter c
swirl r = invWarp (swirlP (-r))

-- Figures from Conal Elliot's Functional Images paper.
fig1 = renderRegion $ vstrip
fig2 = renderRegion $ checker
fig3 = renderRegion $ altRings
fig4 = renderRegion $ polarChecker 10
fig5 = renderRegion $ uscale 0.05 gasket
fig6 = renderFrac   $ wavDist

fig8 = lerpI wavDist (blackWhiteIm (polarChecker 10))
                     (blueYellowIm checker)

fig9 = ybRings
fig10 = renderRegion  udisk
fig11 = renderRegion $ swirl 1 vstrip
disks = renderRegion $ f <$> udisk
                         <*> translate (2,2) udisk
                         <*> translate (3,2) udisk
                         <*> vstrip
  where f a b c d = a || b || c || d

mainImage = fig11

calculate extract = floor . (* 255) . extract . adjust mainImage . toFloat
  where
    (screenWidth, screenHeight) = (640, 480)
    toFloat :: (CInt, CInt) -> (Float, Float)
    toFloat (x,y) = (fromIntegral x, fromIntegral y)
    adjust = adjustToWindow
    adjustToWindow :: Filter c
    adjustToWindow = translate (screenWidth / 2, screenHeight / 2)
                   . scale (60,60)
                   . flipY
    flipY p (x,y) = p (x,-y)

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
vstrip (x,y) = abs x <= 1/2

overlay :: ImageC -> ImageC -> ImageC
overlay = liftA2 (<>)

gasket :: Region
gasket (x,y) = floor x .|. floor y == (floor x :: Integer)

altRings p = even (floor (distO p))

distO (x,y) = sqrt (x * x + y * y)

type Frac = Float -- in [0, 1]

wavDist :: Image Frac
wavDist p = (1 + cos (pi * distO p)) / 2

ybRings = lerpI wavDist blueI yellowI

renderFrac :: Image Frac -> ImageC
renderFrac f (x,y) = lerpC (f (x,y)) black white

lerpC :: Frac -> Color -> Color -> Color
lerpC w (Color r1 g1 b1) (Color r2 g2 b2) = Color (h r1 r2) (h g1 g2) (h b1 b2)
  where
    h x1 x2 = w * x1 + (1 - w) * x2

lighten x c = lerpC x c white
darken  x c = lerpC x c black


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


