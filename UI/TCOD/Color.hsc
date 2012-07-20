{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module UI.TCOD.Color
       ( Color
       , ColorName(..)
       , ColorLevel(..)
       , colorRGB
       , colorHSV
       , add
       , sub
       , mul
       , mul_s
       , lerp
       , getHSV
       , getRGB
       , shiftHue
       , scaleHSV
       , genMap
       , getColor
       ) where

import Data.Int
import Data.Word(Word8(..))

import Foreign hiding (unsafeLocalState)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

#include "color.h"

data Color = Color { colorR, colorG, colorB :: #type uint8 }
           deriving (Show)

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable Color where
  sizeOf _ = #{size TCOD_color_t}
  alignment _ = #{alignment TCOD_color_t}
  peek ptr = do
    r <- #{peek TCOD_color_t, r} ptr
    g <- #{peek TCOD_color_t, g} ptr
    b <- #{peek TCOD_color_t, b} ptr
    return $ Color r g b
  poke ptr (Color r g b) = do
    #{poke TCOD_color_t, r} ptr r
    #{poke TCOD_color_t, g} ptr g
    #{poke TCOD_color_t, b} ptr b

mp3 f g x y z = f (g x) (g y) (g z)
wrapPtr f = unsafeLocalState $ alloca $ \ptr -> do
  f ptr
  peek ptr

-- TCOD_color_t TCOD_color_RGB(uint8 r, uint8 g, uint8 b);
foreign import ccall unsafe "color.h TCOD_color_RGB_ptr"
  tcod_color_RGB :: Word8
                    -> Word8
                    -> Word8
                    -> Ptr Color
                    -> IO ()

colorRGB :: Int8 -> Int8 -> Int8 -> Color
colorRGB r g b = wrapPtr $ (tcod_color_RGB `mp3` fromIntegral) r g b

foreign import ccall unsafe "color.h TCOD_color_HSV_ptr"
  tcod_color_HSV :: CFloat
                    -> CFloat
                    -> CFloat
                    -> Ptr Color
                    -> IO ()

colorHSV :: Float -> Float -> Float -> Color
colorHSV h s v = wrapPtr $ (tcod_color_HSV `mp3` CFloat) h s v

foreign import ccall unsafe "color.h TCOD_color_equals_ptr"
  tcod_color_equals :: Ptr Color
                       -> Ptr Color
                       -> Bool

instance Eq Color where
  (==) c1 c2 = unsafeLocalState $
               alloca $ \p1 ->
               alloca $ \p2 -> do
                 poke p1 c1
                 poke p2 c2
                 return $ tcod_color_equals p1 p2

foreign import ccall unsafe "color.h TCOD_color_add_ptr"
  tcod_color_add :: Ptr Color
                    -> Ptr Color
                    -> Ptr Color
                    -> IO ()

add :: Color -> Color -> Color
add c1 c2 = unsafeLocalState $
            alloca $ \p1 ->
            alloca $ \p2 ->
            alloca $ \res -> do
              poke p1 c1
              poke p2 c2
              tcod_color_add p1 p2 res
              peek res

foreign import ccall unsafe "color.h TCOD_color_subtract_ptr"
  tcod_color_subtract :: Ptr Color
                         -> Ptr Color
                         -> Ptr Color
                         -> IO ()

sub :: Color -> Color -> Color
sub c1 c2 = unsafeLocalState $
            alloca $ \p1 ->
            alloca $ \p2 ->
            alloca $ \res -> do
              poke p1 c1
              poke p2 c2
              tcod_color_subtract p1 p2 res
              peek res

foreign import ccall unsafe "color.h TCOD_color_multiply_ptr"
  tcod_color_multiply :: Ptr Color
                         -> Ptr Color
                         -> Ptr Color
                         -> IO ()

mul :: Color -> Color -> Color
mul c1 c2 = unsafeLocalState $
            alloca $ \p1 ->
            alloca $ \p2 ->
            alloca $ \res -> do
              poke p1 c1
              poke p2 c2
              tcod_color_multiply p1 p2 res
              peek res

foreign import ccall unsafe "color.h TCOD_color_multiply_scalar_ptr"
  tcod_color_multiply_scalar :: Ptr Color
                                -> CFloat
                                -> Ptr Color
                                -> IO ()

mul_s :: Color -> Float -> Color
mul_s c1 f = unsafeLocalState $
             alloca $ \p1 ->
             alloca $ \res -> do
               poke p1 c1
               tcod_color_multiply_scalar p1 (CFloat f) res
               peek res

foreign import ccall unsafe "color.h TCOD_color_lerp_ptr"
  tcod_color_lerp :: Ptr Color
                     -> Ptr Color
                     -> CFloat
                     -> Ptr Color
                     -> IO ()

lerp :: Color -> Color -> Float -> Color
lerp c1 c2 l = unsafeLocalState $
               alloca $ \p1 ->
               alloca $ \p2 ->
               alloca $ \res -> do
                 poke p1 c1
                 poke p2 c2
                 tcod_color_lerp p1 p2 (CFloat l) res
                 peek res

-- | Returns the RGB triple of a color
getRGB :: Color -> (Int8, Int8, Int8)
getRGB (Color r g b) = (fromIntegral r, fromIntegral g, fromIntegral b)

foreign import ccall unsafe "color.h TCOD_color_get_HSV_ptr"
  tcod_color_get_hsv :: Ptr Color
                        -> Ptr CFloat
                        -> Ptr CFloat
                        -> Ptr CFloat
                        -> IO ()

getHSV :: Color -> (Float, Float, Float)
getHSV c = unsafeLocalState $
           alloca $ \p ->
           alloca $ \h ->
           alloca $ \s ->
           alloca $ \v -> do
             poke p c
             tcod_color_get_hsv p h s v
             h' <- peek h
             s' <- peek s
             v' <- peek v
             return (realToFrac h', realToFrac s', realToFrac v')

foreign import ccall unsafe "color.h TCOD_color_shift_hue"
  tcod_color_shift_hue :: Ptr Color
                          -> CFloat
                          -> IO ()

shiftHue :: Color -> Float -> Color
shiftHue c f = unsafeLocalState $
               alloca $ \p -> do
                 poke p c
                 tcod_color_shift_hue p (CFloat f)
                 peek p

foreign import ccall unsafe "color.h TCOD_color_scale_HSV"
  tcod_color_scale_hsv :: Ptr Color
                          -> CFloat
                          -> CFloat
                          -> IO ()

scaleHSV :: Color -> Float -> Float -> Color
scaleHSV c f1 f2 = unsafeLocalState $
                   alloca $ \p -> do
                     poke p c
                     tcod_color_scale_hsv p (CFloat f1) (CFloat f2)
                     peek p

foreign import ccall unsafe "color.h TCOD_color_gen_map"
  tcod_color_gen_map :: Ptr Color
                        -> CInt
                        -> Ptr Color
                        -> Ptr CInt
                        -> IO()

genMap :: [(Color, Int)] -> [Color]
genMap is = unsafeLocalState $
            allocaArray (length is) $ \carr -> -- Color array
            allocaArray (length is) $ \iarr -> -- Index array
            allocaArray resLength $ \oarr -> do -- Out array
              pokeArray carr (map fst is)
              pokeArray iarr (map (fromIntegral . snd) is)
              tcod_color_gen_map oarr (fromIntegral resLength) carr iarr
              peekArray resLength oarr
  where resLength = maximum $ map snd is

data ColorName = Red
               | Flame
               | Orange
               | Amber
               | Yellow
               | Lime
               | Chartreuse
               | Green
               | Sea
               | Turquoise
               | Cyan
               | Sky
               | Azure
               | Blue
               | Han
               | Violet
               | Purple
               | Fuchsia
               | Magenta
               | Pink
               | Crimson
               deriving (Show, Eq)

nameToIndex Red = #const TCOD_COLOR_RED
nameToIndex Flame = #const TCOD_COLOR_FLAME
nameToIndex Orange = #const TCOD_COLOR_ORANGE
nameToIndex Amber = #const TCOD_COLOR_AMBER
nameToIndex Yellow = #const TCOD_COLOR_YELLOW
nameToIndex Lime = #const TCOD_COLOR_LIME
nameToIndex Chartreuse = #const TCOD_COLOR_CHARTREUSE
nameToIndex Green = #const TCOD_COLOR_GREEN
nameToIndex Sea = #const TCOD_COLOR_SEA
nameToIndex Turquoise = #const TCOD_COLOR_TURQUOISE
nameToIndex Cyan = #const TCOD_COLOR_CYAN
nameToIndex Sky = #const TCOD_COLOR_SKY
nameToIndex Azure = #const TCOD_COLOR_AZURE
nameToIndex Blue = #const TCOD_COLOR_BLUE
nameToIndex Han = #const TCOD_COLOR_HAN
nameToIndex Violet = #const TCOD_COLOR_VIOLET
nameToIndex Purple = #const TCOD_COLOR_PURPLE
nameToIndex Fuchsia = #const TCOD_COLOR_FUCHSIA
nameToIndex Magenta = #const TCOD_COLOR_MAGENTA
nameToIndex Pink = #const TCOD_COLOR_PINK
nameToIndex Crimson = #const TCOD_COLOR_CRIMSON

colorNamesLength = #const TCOD_COLOR_NB

data ColorLevel = Desaturated
                | Lightest
                | Lighter
                | Light
                | Normal
                | Dark
                | Darker
                | Darkest
                deriving (Show, Eq)

levelToIndex Desaturated = #const TCOD_COLOR_DESATURATED
levelToIndex Lightest = #const TCOD_COLOR_LIGHTEST
levelToIndex Lighter = #const TCOD_COLOR_LIGHTER
levelToIndex Light = #const TCOD_COLOR_LIGHT
levelToIndex Normal = #const TCOD_COLOR_NORMAL
levelToIndex Dark = #const TCOD_COLOR_DARK
levelToIndex Darker = #const TCOD_COLOR_DARKER
levelToIndex Darkest = #const TCOD_COLOR_DARKEST

colorLevelsLength = #const TCOD_COLOR_LEVELS

foreign import ccall "color.h &TCOD_colors"
  tcod_colors :: Ptr Color

getColor :: ColorName -> ColorLevel -> Color
getColor n l = unsafeLocalState . peek $ ptr
  where ptr = tcod_colors `advancePtr` ptrOffset
        ptrOffset = (nameToIndex n)*colorNamesLength + levelToIndex l
