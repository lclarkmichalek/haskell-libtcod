{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module UI.TCOD.Console
       ( Renderer(..)
       , ConsoleConfig (..)
       , defaultConsoleConfig
       , initConsole

       , FontFlag(..)
       , FontConfig(..)
       , defaultFontConfig
       , setCustomFont

       , mapASCIICodeToFont
       , mapASCIICodesToFont
       , mapStringToFont

       , isFullscreen
       , setFullscreen

       , setWindowTitle
       , isWindowClosed

       , credits
       , creditsRender
       , creditsReset
       ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Data.Char (ord)

#include "libtcod/libtcod.h"

{- The renderer enum type, defined in libtcod/console_types.h. Defines
   the renderer to be used. If the hardware does not support a
   render, libtcod will work it's way down the list of renders till
   it finds one that is supported.

   Using GLSL is recommended, and can have a 9x increase in speed on
   recent cards. -}
data Renderer = RenderGLSL
              | RenderOpenGL
              | RenderSDL
              | RenderNone
              deriving (Eq, Show)

renderToCInt RenderGLSL = #const TCOD_RENDERER_GLSL
renderToCInt RenderOpenGL = #const TCOD_RENDERER_OPENGL
renderToCInt RenderSDL = #const TCOD_RENDERER_SDL
renderToCInt RenderNone = #const TCOD_NB_RENDERERS

foreign import ccall "libtcod/libtcod.h TCOD_console_init_root"
  tcod_console_init_root :: CInt
                         -> CInt
                         -> CString
                         -> Bool
                         -> CInt
                         -> IO ()

-- Configuration for the tcod console.
data ConsoleConfig = ConsoleConfig
                     { consoleFullscreen :: Bool
                     , consoleRenderer :: Renderer
                     } deriving (Eq, Show)

-- The default tcod console config, as defined in tcod C++ headers.
defaultConsoleConfig = ConsoleConfig False RenderGLSL

{- Initialises the root tcod console. `width` and `height` are the
   width and height of the console in characters, as defined by the
   console font (the default font uses 8x8 pixel characters). -}
initConsole :: Int -> Int -> String -> ConsoleConfig -> IO()
initConsole width height windowName config = do
  windowName' <- newCAString windowName
  let width' = fromIntegral width
      height' = fromIntegral height
      fullscreen = consoleFullscreen config
      renderer = renderToCInt $ consoleRenderer config
  tcod_console_init_root width' height' windowName' fullscreen renderer

foreign import ccall "libtcod/libtcod.h TCOD_console_set_custom_font"
  tcod_console_set_custom_font :: CString
                                  -> FontFlag
                                  -> CInt
                                  -> CInt
                                  -> IO ()

-- The font flags, as defined in libtcod/console_types.h. Used to
-- define the font layout, and the font type.
newtype FontFlag = FontFlag { unFontFlag :: CInt }
                 deriving (Eq, Show)

#{enum FontFlag, FontFlag
 , fontLayoutASCIICol = TCOD_FONT_LAYOUT_ASCII_INCOL
 , fontLayoutASCIIRow = TCOD_FONT_LAYOUT_ASCII_INROW
 , fontTypeGreyscale = TCOD_FONT_TYPE_GREYSCALE
 , fontLayoutTCOD = TCOD_FONT_LAYOUT_TCOD
 }

data FontConfig = FontConfig
                  { fontFlags :: FontFlag
                  , fontCharsHorizontal :: Int
                  , fontCharsVertical :: Int
                  } deriving (Eq, Show)

-- The default font config, as specified in the official libtcod C++ bindings
defaultFontConfig :: FontConfig
defaultFontConfig = FontConfig fontLayoutASCIICol 0 0

{- Loads a custom font file to be used on the root console. This
   should be called before initialising the root console with
   `initConsole `-}
setCustomFont :: String -> FontConfig -> IO()
setCustomFont filename config = do
  filename' <- newCAString filename
  let flags = fontFlags config
      ncHz = fromIntegral $ fontCharsHorizontal config
      ncVt = fromIntegral $ fontCharsVertical config
  tcod_console_set_custom_font filename' flags ncHz ncVt

foreign import ccall "libtcod/libtcod.h TCOD_console_map_ascii_code_to_font"
  tcod_console_map_ascii_font :: CInt
                                 -> CInt
                                 -> CInt
                                 -> IO()

-- Maps an ASCII code to a coordinate on the current font. Takes the
-- code to be mapped, and a coordinate.
mapASCIICodeToFont :: Char -> Int -> Int -> IO()
mapASCIICodeToFont c x y =
  tcod_console_map_ascii_font (fromIntegral (ord c))
  (fromIntegral x) (fromIntegral y)

foreign import ccall "libtcod/libtcod.h TCOD_console_map_ascii_codes_to_font"
  tcod_console_map_ascii_fonts :: CInt
                                  -> CInt
                                  -> CInt
                                  -> CInt
                                  -> IO()

-- Maps consecutive ASCII codes to consecutive characters in the
-- current font. Takes the first character to be mapped, the number
-- of characters to be mapped, and the coordinate of the first character
mapASCIICodesToFont :: Char -> Int -> Int -> Int -> IO()
mapASCIICodesToFont c n x y =
  tcod_console_map_ascii_fonts (fromIntegral (ord c)) (fromIntegral n)
  (fromIntegral x) (fromIntegral y)

foreign import ccall "libtcod/libtcod.h TCOD_console_map_string_to_font"
  tcod_console_map_string_font :: CString
                                  -> CInt
                                  -> CInt
                                  -> IO()

-- Maps characters in a string to consecutive characters in the
-- current font. Takes the string to be mapped, and the coordinates
-- of the first character.
mapStringToFont :: String -> Int -> Int -> IO()
mapStringToFont s x y= do
  s' <- newCAString s
  tcod_console_map_string_font s' (fromIntegral x) (fromIntegral y)

foreign import ccall "libtcod/libtcod.h TCOD_console_is_fullscreen"
  tcod_console_is_fullscreen :: IO Bool

-- Tests if the root console is fullscreened or not
isFullscreen = tcod_console_is_fullscreen

foreign import ccall "libtcod/libtcod.h TCOD_console_set_fullscreen"
  tcod_console_set_fullscreen :: Bool -> IO()

-- Sets the console to be fullscreen or not.
setFullscreen = tcod_console_set_fullscreen

foreign import ccall "libtcod/libtcod.h TCOD_console_set_window_title"
  tcod_console_set_window_title :: CString  -> IO()

-- Sets the root window title
setWindowTitle :: String -> IO ()
setWindowTitle s = (newCAString s) >>= tcod_console_set_window_title

foreign import ccall "libtcod/libtcod.h TCOD_console_is_window_closed"
  tcod_console_is_window_closed :: IO Bool

-- Returns true if the window is closed. The program should then exit
-- cleanly.
isWindowClosed :: IO Bool
isWindowClosed = tcod_console_is_window_closed

foreign import ccall "libtcod/libtcod.h TCOD_console_credits"
  tcod_console_credits :: IO ()

-- Prints out the tcod credits on the root console.
credits :: IO ()
credits = tcod_console_credits

foreign import ccall "libtcod/libtcod.h TCOD_console_credits_render"
  tcod_console_credits_render :: CInt
                                 -> CInt
                                 -> Bool
                                 -> IO Bool

-- Prints out the tcod credits at the specified position,
-- transparently if alpha is true. Returns true when the credits have
-- finished.
creditsRender :: Int -> Int -> Bool -> IO Bool
creditsRender x y a = tcod_console_credits_render
                      (fromIntegral x) (fromIntegral y) a

foreign import ccall "libtcod/libtcod.h TCOD_console_credits_reset"
  tcod_console_credits_reset :: IO ()

-- Resets the credits that have been started by `creditsRender`
creditsReset :: IO()
creditsReset = tcod_console_credits_reset