{-# OPTIONS_GHC -fasm #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Picture where

import Foreign.C.Types

foreign export ccall calcR :: CInt -> CInt -> CUChar
foreign export ccall calcG :: CInt -> CInt -> CUChar
foreign export ccall calcB :: CInt -> CInt -> CUChar

calcR x y = fromIntegral $ x + 10
calcG x y = fromIntegral $ y + 23
calcB x y = fromIntegral $ x + 2
