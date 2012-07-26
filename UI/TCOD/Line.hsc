{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-|
  Wraps the line subsystem which implements the Bresenham line drawing
  algorithm.
-}
module UI.TCOD.Line
       ( mapLine
       , getLinePoints
       ) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Unsafe

foreign import ccall unsafe "libtcod.h TCOD_line_init"
  init_line :: CInt ->
               CInt ->
               CInt ->
               CInt ->
               IO ()

foreign import ccall unsafe "libtcod.h TCOD_line_step"
  line_step :: Ptr CInt ->
               Ptr CInt ->
               IO Bool

-- | Maps a function over all the points one the line between the
--   first and second passed points.
mapLine :: (Int, Int) -> (Int, Int) -> ((Int, Int) -> a) -> [a]
mapLine (fx, fy) (sx, sy) f =
  unsafeLocalState $
  init_line fx' fy' sx' sy' >>
  doloop f
  where doloop f =
          alloca $ \xp ->
          alloca $ \yp -> do
            notFin <- line_step xp yp
            x <- peek xp
            y <- peek yp
            let args = (fromIntegral x, fromIntegral y)
            rest <- if notFin
                    then return []
                    else doloop f
            return (f args : rest)
        c = CInt . fromIntegral
        fx' = c fx
        fy' = c fy
        sx' = c sx
        sy' = c sy

-- | Returns the points on the line between the first and second
--   passed points. Implemented by passing id to 'mapLine'
getLinePoints :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getLinePoints s e = mapLine s e id