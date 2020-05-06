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

data Color = Color {r,g,b,a :: {-# UNPACK #-} !Float} deriving Show
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

cOver :: Color -> Color -> Color
cOver (Color r1 g1 b1 a1) (Color r2 g2 b2 a2) = Color (h r1 r2)
                                                      (h g1 g2)
                                                      (h b1 b2)
                                                      (h a1 a2)
  where
    h x1 x2 = x1 + (1 - a1) * x2

bilerpBRBW = bilerpC black red blue white

bilerpC ll lr ul ur (wx, wy) = lerpC wy (lerpC wx ll lr) (lerpC wx ul ur)

over :: ImageC -> ImageC -> ImageC
(top `over` bot) p = top p `cOver` bot p

instance Semigroup Color where
  (<>) = cOver

instance Monoid Color where
  mempty = invisible

invisible = Color 0 0 0 0
black     = Color 0 0 0 1
white     = Color 1 1 1 1
red       = Color 1 0 0 1
green     = Color 0 1 0 1
blue      = Color 0 0 1 1
yellow    = Color 1 1 0 1

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

square n (x,y) = abs x <= n/2 && abs y <= n/2

-- Figures from Conal Elliot's Functional Images paper.
fig1 = renderRegion $ vstrip
fig2 = renderRegion $ checker
fig3 = renderRegion $ altRings
fig4 = renderRegion $ polarChecker 10
fig5 = renderRegion $ uscale 0.05 gasket
fig6 = renderFrac   $ wavDist
fig7 = bilerpBRBW
fig8 = lerpI wavDist (blackWhiteIm (polarChecker 10))
                     (blueYellowIm checker)

fig9 = ybRings
fig10 = renderRegion  udisk
fig11 = renderRegion $ swirl 1 vstrip
fig14 = renderRegion $ annulus 0.5
fig17 = renderRegion $ shiftXor 2.6 altRings
fig18 = renderRegion $ xorgon 8 (7/4) altRings
-- Typo in original paper, 0.2 5 should be 0.25
fig19 = crop (wedgeAnnulus 0.25 10) ybRings
fig20 = crop (swirl 2 (wedgeAnnulus 0.25 10)) ybRings
fig22 = translate (-20, -20) tiledBilerp
fig23 = renderRegion $ uscale 3.5 (radInvert checker)

disks = renderRegion $ f <$> udisk
                         <*> translate (2,2) udisk
                         <*> translate (3,2) udisk
                         <*> vstrip
  where f a b c d = a || b || c || d

mainImage = fig23

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
    f (Color x _ _ _) = x

calcG :: CInt -> CInt -> CUChar
calcG x y = calculate f (x,y)
  where
    f (Color _ y _ _) = y

calcB :: CInt -> CInt -> CUChar
calcB x y = calculate f (x,y)
  where
    f (Color _ _ z _) = z

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
lerpC w (Color r1 g1 b1 a1) (Color r2 g2 b2 a2) =
        Color (h r1 r2) (h g1 g2) (h b1 b2) (h a1 a2)
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

-- Animations
type Time = Float
type Anim c = Time -> Image c
swirlingXPos :: Anim Bool
swirlingXPos t = swirl (t * t) xPos

xPos :: Region
xPos (x,y) = x > 0

-- Region algebra
intersect :: Region -> Region -> Region
intersect = liftA2 (&&)
union :: Region -> Region -> Region
union = liftA2 (||)

xorR :: Region -> Region -> Region
xorR = liftA2 xor
  where xor True b = not b
        xor _    b = b

compR :: Region -> Region
compR = fmap not

universeR :: Region
universeR = const True

emptyR :: Region
emptyR = const False

r \\ r' = r `intersect` compR r'

annulus :: Frac -> Region
annulus inner = udisk \\ uscale inner udisk

radReg :: Int -> Region
radReg n = test . toPolar
  where
    test (r, a) = even (floor (a * fromIntegral n / pi))

wedgeAnnulus :: Frac -> Int -> Region
wedgeAnnulus inner n = annulus inner `intersect` radReg n

shiftXor :: Float -> Region -> Region
shiftXor r reg = reg' r `xorR` reg' (- r)
  where
    reg' d = translate (d, 0) reg

-- Typo in the original paper, missing argument g
xorgon :: Int -> Float -> Region -> Region
xorgon n r g = xorRs (rf <$> [0..n-1])
  where
    rf :: Int -> Region
    rf i = translate (fromPolar (r, a)) g
      where
        a = fromIntegral i * 2 * pi / fromIntegral n

xorRs :: [Region] -> Region
xorRs = foldr xorR emptyR

tileP :: Vector -> Warp
tileP (w,h )(x,y) = (wrap' w x, wrap' h y)

tiledBilerp = about (1/2, 1/2) (tile (1,1)) bilerpBRBW

type HyperFilter c = Filter c -> Filter c

about :: Point -> HyperFilter c
about (x,y) filt = translate (x,y) . filt . translate (-x, -y)

wrap :: Float -> Float -> Float
wrap w x = w * fracPart (x/w)
  where
    fracPart t = t - fromIntegral (truncate t)

wrap' :: Float -> Float -> Float
wrap' w x = wrap w (x + w/2) - w/2

tile :: Vector -> Filter c
tile size = invWarp (tileP size)

swirlP' r = polarWarp (\(ρ, θ) -> (ρ, θ + ρ * (2*pi/r)))
polarWarp warp = fromPolar . warp . toPolar

radInvertP :: Warp
radInvertP = polarWarp (\(ρ,θ) -> (1/ρ, θ))

radInvert :: Filter c
radInvert = invWarp radInvertP
