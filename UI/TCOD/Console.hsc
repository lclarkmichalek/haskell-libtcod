{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-|
  The console subsystem deals with fairly low level drawing operations
  onto the console. Due to the fact that the libtcod console
  implementation relies heavily on mutable state, almost all functions
  in this module are in the IO monad. The documentation for the
  console subsystem can be found here:
  <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console.html?c=true>
-}
module UI.TCOD.Console
       ( Console(..)
       , ConsoleStruct
       , rootConsole

       -- * Initialising the console
       , Renderer(..)
       , ConsoleConfig (..)
       , defaultConsoleConfig
       , initConsole
       , deleteConsole

       -- * Loading custom fonts
       , FontFlag(..)
       , FontConfig(..)
       , defaultFontConfig
       , setCustomFont

       , mapASCIICodeToFont
       , mapASCIICodesToFont
       , mapStringToFont

       -- * Interacting with the window manager
       , isFullscreen
       , setFullscreen

       , setWindowTitle
       , isWindowClosed

       -- * Displaying credits
       , credits
       , creditsRender
       , creditsReset

        -- * Basic drawing functions
       , setDefaultBackground
       , setDefaultForeground
       , clear

       , setCharBackground
       , setCharForeground
       , setChar
       , putChar_
       , putCharEx

       -- * String drawing functions
       , setBackgroundFlag
       , getBackgroundFlag
       , setAlignment
       , getAlignment
       , printString
       , printStringEx
       , printStringRect
       , printStringRectEx
       , computeHeightRect

       -- * Advanced drawing functions
       , rect
       , hLine
       , vLine
       , printFrame

       -- * Getting console information
       , getWidth
       , getHeight
       , getDefaultBackground
       , getDefaultForeground
       , getCharBackground
       , getCharForeground
       , getChar

       -- * Fading functions
       , setFade
       , getFade
       , getFadingColor

       -- * Flushing the root console
       , flushConsole

       -- * Getting input
       , waitForKeypress
       , checkForKeypress
       , isKeyPressed

       -- * Offscreen consoles
       , initOffscreen
       , initOffscreenFromFile
       , loadASC
       , loadAPF
       , saveASC
       , saveAPF

       -- * Blitting
       , blit
       , setKeyColor
       ) where

import Foreign hiding (unsafeLocalState)
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Unsafe (unsafeLocalState)

import Data.Char (ord, chr)

import UI.TCOD.Color(Color(..))
import UI.TCOD.Console.Types

#include "cconsole.h"

data ConsoleStruct = ConsoleStruct

-- | An opaque wrapper around the libtcod console type. Represents
--   both the root console and offscreen consoles
data Console = Console (ForeignPtr ConsoleStruct)

withConsolePtr :: Console -> (Ptr ConsoleStruct -> IO b) -> IO b
withConsolePtr (Console forp) f = withForeignPtr forp f

rootConsole :: IO Console
rootConsole = Console `fmap` (newForeignPtr_ nullPtr)

foreign import ccall "cconsole.h &TCOD_console_delete_ptr"
  tcod_console_delete :: FunPtr (Ptr ConsoleStruct -> IO ())

--Creates a finalised console from a raw console pointer
createConsole :: IO (Ptr ConsoleStruct) -> IO Console
createConsole rawC = do
  rawC' <- rawC
  fPtr <- newForeignPtr (tcod_console_delete) rawC'
  return $ Console fPtr

-- | Deletes the current console. This function is idempotent.
deleteConsole :: Console -> IO ()
deleteConsole (Console forp) = finalizeForeignPtr forp

foreign import ccall "cconsole.h TCOD_console_init_root"
  tcod_console_init_root :: CInt
                         -> CInt
                         -> CString
                         -> Bool
                         -> CInt
                         -> IO ()

-- | Configuration for the root tcod console.
data ConsoleConfig = ConsoleConfig
                     { consoleFullscreen :: Bool
                     , consoleRenderer :: Renderer
                     } deriving (Eq, Show)

-- | The default tcod console config, as defined in tcod C++ headers.
defaultConsoleConfig = ConsoleConfig False renderGLSL

{-|
  Initialises the root tcod console. `width` and `height` are the
  width and height of the console in characters, as defined by the
  console font (the default font uses 8x8 pixel characters).

  Wraps <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_init_root.html?c=true>
-}
initConsole :: Int -> Int -> String -> ConsoleConfig -> IO Console
initConsole width height windowName config = do
  windowName' <- newCAString windowName
  let width' = fromIntegral width
      height' = fromIntegral height
      fullscreen = consoleFullscreen config
      renderToCInt (Renderer c) = c
      renderer = renderToCInt $ consoleRenderer config
  tcod_console_init_root width' height' windowName' fullscreen renderer
  rootConsole

foreign import ccall "cconsole.h TCOD_console_set_custom_font"
  tcod_console_set_custom_font :: CString
                                  -> FontFlag
                                  -> CInt
                                  -> CInt
                                  -> IO ()

-- | Configuration for loading custom fonts
data FontConfig = FontConfig
                  { fontFlags :: FontFlag
                  , fontCharsHorizontal :: Int
                  , fontCharsVertical :: Int
                  } deriving (Eq, Show)

-- | The default font config, as specified in the libtcod C++ bindings
defaultFontConfig :: FontConfig
defaultFontConfig = FontConfig fontLayoutASCIICol 0 0

{-|
  Loads a custom font file to be used on the root console. This
  should be called before initialising the root console with
  'initConsole'

  Wraps <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_set_custom_font.html?c=true>
-}
setCustomFont :: String -> FontConfig -> IO()
setCustomFont filename config = do
  filename' <- newCAString filename
  let flags = fontFlags config
      ncHz = fromIntegral $ fontCharsHorizontal config
      ncVt = fromIntegral $ fontCharsVertical config
  tcod_console_set_custom_font filename' flags ncHz ncVt

foreign import ccall "cconsole.h TCOD_console_map_ascii_code_to_font"
  tcod_console_map_ascii_font :: CInt
                                 -> CInt
                                 -> CInt
                                 -> IO()

-- | Maps an ASCII code to a coordinate on the current font. Takes the
--   code to be mapped, and a coordinate. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_map.html?c=true#0>
mapASCIICodeToFont :: Char -> Int -> Int -> IO()
mapASCIICodeToFont c x y =
  tcod_console_map_ascii_font (fromIntegral (ord c))
  (fromIntegral x) (fromIntegral y)

foreign import ccall "cconsole.h TCOD_console_map_ascii_codes_to_font"
  tcod_console_map_ascii_fonts :: CInt
                                  -> CInt
                                  -> CInt
                                  -> CInt
                                  -> IO()

-- | Maps consecutive ASCII codes to consecutive characters in the
--   current font. Takes the first character to be mapped, the number
--   of characters to be mapped, and the coordinate of the first character.
--   Wraps <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_map.html?c=true#1>
mapASCIICodesToFont :: Char -> Int -> Int -> Int -> IO()
mapASCIICodesToFont c n x y =
  tcod_console_map_ascii_fonts (fromIntegral (ord c)) (fromIntegral n)
  (fromIntegral x) (fromIntegral y)

foreign import ccall "cconsole.h TCOD_console_map_string_to_font"
  tcod_console_map_string_font :: CString
                                  -> CInt
                                  -> CInt
                                  -> IO()

-- | Maps characters in a string to consecutive characters in the
--   current font. Takes the string to be mapped, and the coordinates
--   of the first character. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_map.html?c=true#2>
mapStringToFont :: String -> Int -> Int -> IO()
mapStringToFont s x y= do
  s' <- newCAString s
  tcod_console_map_string_font s' (fromIntegral x) (fromIntegral y)

foreign import ccall "cconsole.h TCOD_console_is_fullscreen"
  tcod_console_is_fullscreen :: IO Bool

-- | Tests if the root console is fullscreened or not. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_fullscreen.html?c=true#0>
isFullscreen = tcod_console_is_fullscreen

foreign import ccall "cconsole.h TCOD_console_set_fullscreen"
  tcod_console_set_fullscreen :: Bool -> IO()

-- | Sets the console to be fullscreen or not. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_fullscreen.html?c=true#1>
setFullscreen = tcod_console_set_fullscreen

foreign import ccall "cconsole.h TCOD_console_set_window_title"
  tcod_console_set_window_title :: CString  -> IO()

-- | Sets the root window title. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_window.html?c=true#0>
setWindowTitle :: String -> IO ()
setWindowTitle s = (newCAString s) >>= tcod_console_set_window_title

foreign import ccall "cconsole.h TCOD_console_is_window_closed"
  tcod_console_is_window_closed :: IO Bool

-- | Returns true if the window is closed. The program should then exit
--   cleanly. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_window.html?c=true#1>
isWindowClosed :: IO Bool
isWindowClosed = tcod_console_is_window_closed

foreign import ccall "cconsole.h TCOD_console_credits"
  tcod_console_credits :: IO ()

-- | Prints out the tcod credits onto the root console. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_credits.html?c=true#0>
credits :: IO ()
credits = tcod_console_credits

foreign import ccall "cconsole.h TCOD_console_credits_render"
  tcod_console_credits_render :: CInt
                                 -> CInt
                                 -> Bool
                                 -> IO Bool

-- | Prints out the tcod credits at the specified position,
--   transparently if alpha is true. Returns true when the credits have
--   finished. Wraps
--   <doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_credits.html?c=true#1>
creditsRender :: (Int, Int) -> Bool -> IO Bool
creditsRender (x, y) a = tcod_console_credits_render
                      (fromIntegral x) (fromIntegral y) a

foreign import ccall "cconsole.h TCOD_console_credits_reset"
  tcod_console_credits_reset :: IO ()

-- | Resets the credits that have been started by 'creditsRender'. Wraps
--   <doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_credits.html?c=true#2>
creditsReset :: IO()
creditsReset = tcod_console_credits_reset

foreign import ccall "cconsole.h TCOD_console_set_default_background_ptr"
  tcod_console_set_default_background :: Ptr ConsoleStruct
                                         -> Ptr Color
                                         -> IO ()

-- | Sets the default background colour of the console. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_draw_basic.html?c=true#0>
setDefaultBackground :: Console -> Color -> IO ()
setDefaultBackground con col =
  alloca $ \colp ->
  withConsolePtr con $ \conp -> do
    poke colp col
    tcod_console_set_default_background conp colp

foreign import ccall "cconsole.h TCOD_console_set_default_foreground_ptr"
  tcod_console_set_default_foreground :: Ptr ConsoleStruct
                                         -> Ptr Color
                                         -> IO ()

-- | Sets the default background colour of the console. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_draw_basic.html?c=true#1>
setDefaultForeground :: Console -> Color -> IO ()
setDefaultForeground con col =
  alloca $ \colp ->
  withConsolePtr con $ \conp -> do
    poke colp col
    tcod_console_set_default_foreground conp colp

foreign import ccall "cconsole.h TCOD_console_clear"
  tcod_console_clear :: Ptr ConsoleStruct
                        -> IO ()

-- | Clears the console. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_draw_basic.html?c=true#2>
clear :: Console -> IO ()
clear con = withConsolePtr con $ \conp -> do
  tcod_console_clear conp

foreign import ccall "cconsole.h TCOD_console_set_char_background_ptr"
  tcod_console_set_char_back :: Ptr ConsoleStruct
                                -> CInt
                                -> CInt
                                -> Ptr Color
                                -> CInt
                                -> IO ()

-- | Sets the background color of a cell. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_draw_basic.html?c=true#3>
setCharBackground :: Console -> (Int, Int) -> Color -> BackgroundFlag -> IO ()
setCharBackground con (x, y) col bf =
  withConsolePtr con $ \conp ->
  alloca $ \colp -> do
    poke colp col
    tcod_console_set_char_back conp x' y' colp (unBF bf)
      where unBF (BackgroundFlag c) = c
            x' = CInt (fromIntegral x)
            y' = CInt (fromIntegral y)

foreign import ccall "cconsole.h TCOD_console_set_char_foreground_ptr"
  tcod_console_set_char_fore :: Ptr ConsoleStruct
                                -> CInt
                                -> CInt
                                -> Ptr Color
                                -> IO ()

-- | Sets the foreground color of a cell. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_draw_basic.html?c=true#4>
setCharForeground :: Console -> (Int, Int) -> Color -> IO ()
setCharForeground con (x, y) col =
  withConsolePtr con $ \conp ->
  alloca $ \colp -> do
    poke colp col
    tcod_console_set_char_fore conp x' y' colp
      where x' = CInt (fromIntegral x)
            y' = CInt (fromIntegral y)

foreign import ccall "cconsole.h TCOD_console_set_char"
  tcod_console_set_char :: Ptr ConsoleStruct
                           -> CInt
                           -> CInt
                           -> CInt
                           -> IO ()

-- | Set the ASCII code of the cell. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_draw_basic.html?c=true#5>
setChar :: Console -> (Int, Int) -> Char -> IO ()
setChar con (x, y) char = withConsolePtr con $ \conp -> do
  tcod_console_set_char conp x' y' char'
    where conv = CInt . fromIntegral
          x' = conv x
          y' = conv y
          char' = conv (ord char)

foreign import ccall "cconsole.h TCOD_console_put_char"
  tcod_console_put_char :: Ptr ConsoleStruct
                           -> CInt
                           -> CInt
                           -> CInt
                           -> CInt
                           -> IO ()

-- | Sets all the properties of a cell, using the default foreground color, and
--   modifying the background color according to the 'BackgroundFlag'.
--   Wraps <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_draw_basic.html?c=true#6>
putChar_ :: Console -> (Int, Int) -> Char -> BackgroundFlag -> IO ()
putChar_ con (x, y) char (BackgroundFlag bf) = withConsolePtr con $ \conp -> do
  tcod_console_put_char conp x' y' char' bf
  where conv = CInt . fromIntegral
        x' = conv x
        y' = conv y
        char' = conv (ord char)

foreign import ccall "cconsole.h TCOD_console_put_char_ex_ptr"
  tcod_console_put_char_ex :: Ptr ConsoleStruct
                              -> CInt
                              -> CInt
                              -> CInt
                              -> Ptr Color
                              -> Ptr Color
                              -> IO ()

-- | Sets all the properties of a cell, using specific colors. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_draw_basic.html?c=true#7>
putCharEx :: Console -> (Int, Int) -> Char -> Color -> Color -> IO ()
putCharEx con (x, y) char for bak =
  withConsolePtr con $ \conp ->
  alloca $ \forp ->
  alloca $ \bakp -> do
    poke forp for
    poke bakp bak
    tcod_console_put_char_ex conp x' y' char' forp bakp
    where conv = CInt . fromIntegral
          x' = conv x
          y' = conv y
          char' = conv (ord char)

foreign import ccall unsafe "cconsole.h TCOD_console_set_background_flag"
  tcod_console_set_background_flag :: Ptr ConsoleStruct
                                      -> CInt
                                      -> IO ()

-- | Sets the default background flag for a console. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_print.html?c=true#0>
setBackgroundFlag :: Console -> BackgroundFlag -> IO ()
setBackgroundFlag con (BackgroundFlag bf) =
  withConsolePtr con (flip tcod_console_set_background_flag bf)

foreign import ccall unsafe "cconsole.h TCOD_console_get_background_flag"
  tcod_console_get_background_flag :: Ptr ConsoleStruct
                                      -> IO CInt

-- | Returns the default background flag of a console. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_print.html?c=true#1>
getBackgroundFlag :: Console -> IO BackgroundFlag
getBackgroundFlag  con =
  BackgroundFlag `fmap`
  (withConsolePtr con tcod_console_get_background_flag)

foreign import ccall unsafe "cconsole.h TCOD_console_set_alignment"
  tcod_console_set_alignment :: Ptr ConsoleStruct
                                -> CInt
                                -> IO ()

-- | Sets the default alignment of a console. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_print.html?c=true#2>
setAlignment :: Console -> Alignment -> IO ()
setAlignment con (Alignment al) =
  withConsolePtr con (flip tcod_console_set_alignment al)

foreign import ccall unsafe "cconsole.h TCOD_console_get_alignment"
  tcod_console_get_alignment :: Ptr ConsoleStruct
                                -> IO CInt

-- | Returns the default alignment of a console. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_print.html?c=true#3>
getAlignment :: Console -> IO Alignment
getAlignment  con =
  Alignment `fmap`
  (withConsolePtr con tcod_console_get_alignment)

foreign import ccall unsafe "cconsole.h TCOD_console_print"
  tcod_console_print :: Ptr ConsoleStruct
                        -> CInt
                        -> CInt
                        -> CString
                        -> IO ()

-- | Prints a string at the given position using the console alignment
--   and background flag. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_print.html?c=true#4>
printString :: Console -> (Int, Int) -> String -> IO ()
printString con (x, y) str =
  withConsolePtr con $ \conp ->
  withCAString str $ \strp ->
  tcod_console_print conp x' y' strp
  where conv = CInt . fromIntegral
        x' = conv x
        y' = conv y

foreign import ccall unsafe "cconsole.h TCOD_console_print_ex"
  tcod_console_print_ex :: Ptr ConsoleStruct
                           -> CInt
                           -> CInt
                           -> CInt
                           -> CInt
                           -> CString
                           -> IO ()

-- | Prints a string at the given position using specific alignment
--   and background flag. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_print.html?c=true#5>
printStringEx :: Console -> (Int, Int) ->
                 BackgroundFlag -> Alignment -> String -> IO ()
printStringEx con (x, y) (BackgroundFlag bf) (Alignment a) str =
  withCAString str $ \strp ->
  withConsolePtr con $ \conp ->
  tcod_console_print_ex conp x' y' bf a strp
  where conv = CInt . fromIntegral
        x' = conv x
        y' = conv y

foreign import ccall unsafe "cconsole.h TCOD_console_print_rect"
  tcod_console_print_rect :: Ptr ConsoleStruct
                             -> CInt
                             -> CInt
                             -> CInt
                             -> CInt
                             -> CString
                             -> IO ()

-- | Prints a string at the given position inside a rectangle of the
--   given width and height. If the string is longer than the width,
--   carriage returns will be inserted. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_print.html?c=true#6>
printStringRect :: Console -> (Int, Int) -> (Int, Int) -> String -> IO ()
printStringRect con (x, y) (w, h) str =
  withConsolePtr con $ \conp ->
  withCAString str $ \strp ->
  tcod_console_print_rect conp x' y' w' h' strp
  where conv = CInt . fromIntegral
        x' = conv x
        y' = conv y
        w' = conv w
        h' = conv h

foreign import ccall unsafe "cconsole.h TCOD_console_print_rect_ex"
  tcod_console_print_rect_ex :: Ptr ConsoleStruct
                                -> CInt
                                -> CInt
                                -> CInt
                                -> CInt
                                -> CInt
                                -> CInt
                                -> CString
                                -> IO ()

-- | Prints a string at the given position inside a rectangle of the
--   given width and height using specific background flag and
--   alignment . If the string is longer than the width,
--   carriage returns will be inserted. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_print.html?c=true#7>
printStringRectEx :: Console -> (Int, Int) -> (Int, Int) ->
                     BackgroundFlag -> Alignment -> String -> IO ()
printStringRectEx con (x, y) (w, h) (BackgroundFlag bf) (Alignment a) str =
  withCAString str $ \strp ->
  withConsolePtr con $ \conp ->
  tcod_console_print_rect_ex conp x' y' w' h' bf a strp
  where c = CInt . fromIntegral
        x' = c x
        y' = c y
        w' = c w
        h' = c h

foreign import ccall unsafe "cconsole.h TCOD_console_get_height_rect"
  tcod_console_get_height_rect :: CInt
                                  -> CInt
                                  -> CInt
                                  -> CInt
                                  -> CString
                                  -> IO CInt

-- | Computes the height of the wrapped string without calling
--   'printStringRect'. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_print.html?c=true#8>
computeHeightRect :: (Int, Int) -> (Int, Int) -> String -> Int
computeHeightRect (x, y) (w, h) str =
  fromIntegral $
  unsafeLocalState $
  withCAString str $ \strp ->
  tcod_console_get_height_rect x' y' w' h' strp
  where c = CInt . fromIntegral
        x' = c x
        y' = c y
        w' = c w
        h' = c h

foreign import ccall "cconsole.h TCOD_console_rect"
  tcod_console_rect :: Ptr ConsoleStruct
                       -> CInt
                       -> CInt
                       -> CInt
                       -> CInt
                       -> Bool
                       -> CInt
                       -> IO ()

-- | Fill a rectangle with a background color. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_advanced.html?c=true#0>
rect :: Console -> (Int, Int) -> (Int, Int) -> Bool -> BackgroundFlag -> IO ()
rect con (x, y) (w, h) c (BackgroundFlag bf) = withConsolePtr con $ \conp -> do
  tcod_console_rect conp x' y' w' h' c bf
  where conv = CInt . fromIntegral
        x' = conv x
        y' = conv y
        w' = conv w
        h' = conv h

foreign import ccall "cconsole.h TCOD_console_hline"
  tcod_console_hline :: Ptr ConsoleStruct
                        -> CInt
                        -> CInt
                        -> CInt
                        -> CInt
                        -> IO ()

-- | Draw a horizontal line using a horizontal bar character. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_advanced.html?c=true#1>
hLine :: Console -> (Int, Int) -> Int -> BackgroundFlag -> IO ()
hLine con (x, y) l (BackgroundFlag bf) = withConsolePtr con $ \conp ->
  tcod_console_hline conp x' y' l' bf
  where conv = CInt . fromIntegral
        x' = conv x
        y' = conv y
        l' = conv l

foreign import ccall "cconsole.h TCOD_console_vline"
  tcod_console_vline :: Ptr ConsoleStruct
                        -> CInt
                        -> CInt
                        -> CInt
                        -> CInt
                        -> IO ()

-- | Draws a vertical line using a vertical bar character. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_advanced.html?c=true#2>
vLine :: Console -> (Int, Int) -> Int -> BackgroundFlag -> IO ()
vLine con (x, y) l (BackgroundFlag bf) = withConsolePtr con $ \conp ->
  tcod_console_vline conp x' y' l' bf
  where conv = CInt . fromIntegral
        x' = conv x
        y' = conv y
        l' = conv l

foreign import ccall "cconsole.h TCOD_console_print_frame"
  tcod_console_print_frame :: Ptr ConsoleStruct
                              -> CInt
                              -> CInt
                              -> CInt
                              -> CInt
                              -> Bool
                              -> CInt
                              -> Ptr ConsoleStruct
                              -> IO ()

-- | Draws a rectangular frame. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_advanced.html?c=true#3>
printFrame :: Console -> (Int, Int) -> (Int, Int)
              -> Bool -> BackgroundFlag -> IO ()
printFrame con (x, y) (w, h) clear (BackgroundFlag bf) =
  withConsolePtr con $ \conp -> do
    tcod_console_print_frame conp x' y' w' h' clear bf nullPtr
  where conv = CInt . fromIntegral
        x' = conv x
        y' = conv y
        w' = conv w
        h' = conv h

foreign import ccall unsafe "cconsole.h TCOD_console_get_width"
  tcod_console_get_width :: Ptr ConsoleStruct
                            -> IO CInt

-- | Returns the width of the console. This function is only in the IO
--   monad as the size of offscreen console can be changed when
--   loading an apf file. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_read.html?c=true#0>
getWidth :: Console -> IO Int
getWidth con = fromIntegral `fmap` (withConsolePtr con tcod_console_get_width)

foreign import ccall unsafe "cconsole.h TCOD_console_get_height"
  tcod_console_get_height :: Ptr ConsoleStruct
                            -> IO CInt

-- | Returns the height of the console. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_read.html?c=true#1>
getHeight :: Console -> IO Int
getHeight con = fromIntegral `fmap` (withConsolePtr con tcod_console_get_height)

foreign import ccall unsafe "cconsole.h TCOD_console_get_default_background_ptr"
  tcod_console_get_default_background :: Ptr ConsoleStruct
                                         -> Ptr Color
                                         -> IO ()

-- | Returns the default background color. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_read.html?c=true#2>
getDefaultBackground :: Console -> IO Color
getDefaultBackground con = withConsolePtr con $ \conp ->
                           alloca $ \colp -> do
                             tcod_console_get_default_background conp colp
                             peek colp

foreign import ccall unsafe "cconsole.h TCOD_console_get_default_foreground_ptr"
  tcod_console_get_default_foreground :: Ptr ConsoleStruct
                                         -> Ptr Color
                                         -> IO ()

-- | Returns the default foreground color. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_read.html?c=true#3>
getDefaultForeground :: Console -> IO Color
getDefaultForeground con = withConsolePtr con $ \conp ->
                           alloca $ \colp -> do
                             tcod_console_get_default_foreground conp colp
                             peek colp

foreign import ccall unsafe "cconsole.h TCOD_console_get_char_background_ptr"
  tcod_console_get_char_background :: Ptr ConsoleStruct
                                      -> CInt
                                      -> CInt
                                      -> Ptr Color
                                      -> IO ()

-- | Returns the background color of a cell. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_read.html?c=true#4>
getCharBackground :: Console -> (Int, Int) -> IO Color
getCharBackground con (x, y) =
  withConsolePtr con $ \conp ->
  alloca $ \colp -> do
    tcod_console_get_char_background conp x' y' colp
    peek colp
    where conv = CInt . fromIntegral
          x' = conv x
          y' = conv y

foreign import ccall unsafe "cconsole.h TCOD_console_get_char_foreground_ptr"
  tcod_console_get_char_foreground :: Ptr ConsoleStruct
                                      -> CInt
                                      -> CInt
                                      -> Ptr Color
                                      -> IO ()

-- | Returns the foreground color of a cell. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_read.html?c=true#5>
getCharForeground :: Console -> (Int, Int) -> IO Color
getCharForeground con (x, y) =
  withConsolePtr con $ \conp ->
  alloca $ \colp -> do
    tcod_console_get_char_foreground conp x' y' colp
    peek colp
    where conv = CInt . fromIntegral
          x' = conv x
          y' = conv y

foreign import ccall unsafe "cconsole.h TCOD_console_get_char"
  tcod_console_get_char :: Ptr ConsoleStruct
                           -> CInt
                           -> CInt
                           -> IO CInt

-- | Returns the ASCII code of a cell. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_read.html?c=true#6>
getChar_ :: Console -> (Int, Int) -> IO Char
getChar_ con (x, y) = withConsolePtr con $ \conp ->
                      tcod_console_get_char conp x' y' >>=
                     (return . chr . fromIntegral)
  where conv = CInt . fromIntegral
        x' = conv x
        y' = conv y

foreign import ccall unsafe "cconsole.h TCOD_console_set_fade_ptr"
  tcod_console_set_fade :: CUChar
                           -> Ptr Color
                           -> IO ()

-- | Sets the fading parameters. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_fading.html?c=true#0>
setFade :: Int -> Color -> IO ()
setFade f c = alloca $ \cp -> do
  poke cp c
  tcod_console_set_fade (fromIntegral f) cp

foreign import ccall unsafe "cconsole.h TCOD_console_get_fade"
  tcod_console_get_fade :: IO CUChar

-- | The fade amount. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_fading.html?c=true#1>
getFade :: IO Int
getFade = tcod_console_get_fade >>= (return . fromIntegral)

foreign import ccall unsafe "cconsole.h TCOD_console_get_fading_color_ptr"
  tcod_console_get_fading_color :: Ptr Color
                                   -> IO ()

-- | The fading color. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_fading.html?c=true#2>
getFadingColor :: IO Color
getFadingColor = alloca $ \p -> do
  tcod_console_get_fading_color p
  peek p

foreign import ccall unsafe "cconsole.h TCOD_console_flush"
  tcod_console_flush :: IO ()

-- | Flushes the root console to the screen. Wraps
--   <doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_flush.html?c=true>
flushConsole :: IO ()
flushConsole = tcod_console_flush

foreign import ccall unsafe "cconsole.h TCOD_console_wait_for_keypress_ptr"
  tcod_console_wait_for_keypress :: Bool
                                    -> Ptr KeyEvent
                                    -> IO ()

-- | Waits for a keypress. If passed True, will flush all pending
--   keypresses. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_blocking_input.html?c=true#0>
--
--   The equivalent function for checking all kinds of event is 'UI.TCOD.System.waitForEvent'
waitForKeypress :: Bool -> IO KeyEvent
waitForKeypress f =
  alloca $ \kep ->
  tcod_console_wait_for_keypress f kep >>
  peek kep

foreign import ccall unsafe "cconsole.h TCOD_console_check_for_keypress_ptr"
  tcod_console_check_for_keypress :: CInt
                                     -> Ptr KeyEvent
                                     -> IO ()

-- | Checks for a keypress. The 'KeyStatus' is the status of key event
--   that should be checked for. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_non_blocking_input.html?c=true#0>
--
--   The equivalent function for checking all kinds of event is 'UI.TCOD.System.checkForEvent'
checkForKeypress :: KeyStatus -> IO (Maybe KeyEvent)
checkForKeypress (KeyStatus s) =
  alloca $ \kep -> do
    tcod_console_check_for_keypress s kep
    ke <- peek kep
    return $ if eventCode ke == kNone
             then Nothing
             else Just ke

foreign import ccall unsafe "cconsole.h TCOD_console_is_key_pressed"
  tcod_console_is_key_pressed :: CInt
                                 -> IO Bool

-- | Returns True if the special key is pressed.
isKeyPressed :: Keycode -> IO Bool
isKeyPressed (Keycode kc) = tcod_console_is_key_pressed kc

foreign import ccall unsafe "cconsole.h TCOD_console_set_keyboard_repeat"
  tcod_console_set_keyboard_repeat :: CInt
                                      -> CInt
                                      -> IO ()

-- | Sets the keyboard repeat interval (the interval at which a
--   held key will trigger a key press event). Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_keyboard_repeat.html?c=true#0>
--
--   To disable keyboard repeat altogether, run
-- @
--   setKeyboardRepeat 0 0
-- @
setKeyboardRepeat :: Int -> Int -> IO ()
setKeyboardRepeat start repeat =
  tcod_console_set_keyboard_repeat (conv start) (conv repeat)
  where conv = CInt . fromIntegral

foreign import ccall unsafe "cconsole.h TCOD_console_new"
  tcod_console_new :: CInt
                      -> CInt
                      -> IO (Ptr ConsoleStruct)

-- | Creates a new offscreen console. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_offscreen.html?c=true#0>
initOffscreen :: Int -> Int -> IO Console
initOffscreen w h = createConsole $ tcod_console_new w' h'
  where conv = CInt . fromIntegral
        w' = conv w
        h' = conv h

foreign import ccall unsafe "cconsole.h TCOD_console_from_file"
  tcod_console_from_file :: CString
                            -> IO (Ptr ConsoleStruct)

-- | Creates a new offscreen console from an apf or asc file. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_offscreen.html?c=true#1>
initOffscreenFromFile :: String -> IO Console
initOffscreenFromFile fname =
  newCAString fname >>=
  createConsole . tcod_console_from_file

foreign import ccall unsafe "cconsole.h TCOD_console_load_asc"
  tcod_console_load_asc :: Ptr ConsoleStruct
                           -> CString
                           -> IO Bool

-- | Loads an ASC file to an offscreen console. Returns False if the
--   file could not be read. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_offscreen.html?c=true#2>
loadASC :: Console -> String -> IO Bool
loadASC con fname =
  withConsolePtr con $ \conp ->
  if conp == nullPtr
  then return False
  else (newCAString fname >>=
        tcod_console_load_asc conp)

foreign import ccall unsafe "cconsole.h TCOD_console_load_apf"
  tcod_console_load_apf :: Ptr ConsoleStruct
                           -> CString
                           -> IO Bool

-- | Loads an APF file to an offscreen console. Returns False if the
--   file could not be read. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_offscreen.html?c=true#3>
loadAPF :: Console -> String -> IO Bool
loadAPF con fname =
  withConsolePtr con $ \conp ->
  if conp == nullPtr
  then return False
  else (newCAString fname >>=
        tcod_console_load_apf conp)

foreign import ccall unsafe "cconsole.h TCOD_console_save_asc"
  tcod_console_save_asc :: Ptr ConsoleStruct
                           -> CString
                           -> IO Bool

-- | Saves an offscreen console to an ASC file. Returns False if the
--   file could not be written. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_offscreen.html?c=true#4>
saveASC :: Console -> String -> IO Bool
saveASC con fname =
  withConsolePtr con $ \conp ->
  newCAString fname >>=
  tcod_console_save_asc conp

foreign import ccall unsafe "cconsole.h TCOD_console_save_apf"
  tcod_console_save_apf :: Ptr ConsoleStruct
                           -> CString
                           -> IO Bool

-- | Saves an offscreen console to an APF file. Returns False if the
--   file could not be written. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_offscreen.html?c=true#5>
saveAPF :: Console -> String -> IO Bool
saveAPF con fname =
  withConsolePtr con $ \conp ->
  newCAString fname >>=
  tcod_console_save_apf conp

foreign import ccall unsafe "cconsole.h TCOD_console_blit"
  tcod_console_blit :: Ptr ConsoleStruct -> CInt -> CInt -> CInt -> CInt
                       -> Ptr ConsoleStruct -> CInt -> CInt
                       -> CFloat -> CFloat
                       -> IO ()

-- | Blits one console onto another. Takes the source console, the
--   top left coordinate of the region to blit, and the width and
--   height of the region to blit, then the destination console and
--   the coordinate to blit to. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_offscreen.html?c=true#6>
blit :: Console -> (Int, Int) -> (Int, Int)
        -> Console -> (Int, Int) -> Float -> Float -> IO ()
blit src (sx, sy) (sw, sh) dst (dx, dy) fga bga =
  withConsolePtr src $ \srcp ->
  withConsolePtr dst $ \dstp ->
  tcod_console_blit srcp sx' sy' sw' sh' dstp dx' dy' fga' bga'
  where i = CInt . fromIntegral
        sx' = i sx
        sy' = i sy
        sw' = i sw
        sh' = i sh
        dx' = i dx
        dy' = i dy
        f = CFloat
        fga' = f fga
        bga' = f bga

foreign import ccall unsafe "cconsole.h TCOD_console_set_key_color_ptr"
  tcod_console_set_key_color :: Ptr ConsoleStruct
                                -> Ptr Color
                                -> IO ()

-- | Sets the key color of a console for blitting. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/console_offscreen.html?c=true#7>
setKeyColor :: Console -> Color -> IO ()
setKeyColor con col =
  withConsolePtr con $ \conp ->
  alloca $ \colp ->
  tcod_console_set_key_color conp colp