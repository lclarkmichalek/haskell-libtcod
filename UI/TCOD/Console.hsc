{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module UI.TCOD.Console
       ( Console(..)

       , Renderer(..)
       , ConsoleConfig (..)
       , defaultConsoleConfig
       , initConsole
       , deleteConsole

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

       , setDefaultBackground
       , setDefaultForeground
       , clear

       , setCharBackground
       , setCharForeground
       , setChar
       , putChar_
       , putCharEx

       , rect
       , hLine
       , vLine
       , printFrame

       , getWidth
       , getHeight
       , getDefaultBackground
       , getDefaultForeground
       , getCharBackground
       , getCharForeground
       , getChar

       , setFade
       , getFade
       , getFadingColor

       , flushConsole

       , initOffscreen
       , initOffscreenFromFile
       , loadASC
       , loadAFP
       , saveASC
       , saveAFP

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

#include "console.h"

data Console = Console (ForeignPtr ())
             | RootConsole

withConsolePtr :: Console -> (Ptr () -> IO b) -> IO b
withConsolePtr (Console forp) f = withForeignPtr forp f
withConsolePtr RootConsole f = f nullPtr

foreign import ccall "console.h &TCOD_console_delete_ptr"
  tcod_console_delete :: FunPtr (Ptr () -> IO ())

-- Creates a finalised console from a raw console pointer
createConsole :: IO (Ptr ()) -> IO Console
createConsole rawC = do
  rawC' <- rawC
  fPtr <- newForeignPtr (tcod_console_delete) rawC'
  return $ Console fPtr

deleteConsole :: Console -> IO ()
deleteConsole RootConsole = return
deleteConsole (Console forp) = finalizeForeignPtr forp

foreign import ccall "console.h TCOD_console_init_root"
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
defaultConsoleConfig = ConsoleConfig False renderGLSL

{- Initialises the root tcod console. `width` and `height` are the
   width and height of the console in characters, as defined by the
   console font (the default font uses 8x8 pixel characters). -}
initConsole :: Int -> Int -> String -> ConsoleConfig -> IO Console
initConsole width height windowName config = do
  windowName' <- newCAString windowName
  let width' = fromIntegral width
      height' = fromIntegral height
      fullscreen = consoleFullscreen config
      renderToCInt (Renderer c) = c
      renderer = renderToCInt $ consoleRenderer config
  tcod_console_init_root width' height' windowName' fullscreen renderer
  return RootConsole

foreign import ccall "console.h TCOD_console_set_custom_font"
  tcod_console_set_custom_font :: CString
                                  -> FontFlag
                                  -> CInt
                                  -> CInt
                                  -> IO ()

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

foreign import ccall "console.h TCOD_console_map_ascii_code_to_font"
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

foreign import ccall "console.h TCOD_console_map_ascii_codes_to_font"
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

foreign import ccall "console.h TCOD_console_map_string_to_font"
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

foreign import ccall "console.h TCOD_console_is_fullscreen"
  tcod_console_is_fullscreen :: IO Bool

-- Tests if the root console is fullscreened or not
isFullscreen = tcod_console_is_fullscreen

foreign import ccall "console.h TCOD_console_set_fullscreen"
  tcod_console_set_fullscreen :: Bool -> IO()

-- Sets the console to be fullscreen or not.
setFullscreen = tcod_console_set_fullscreen

foreign import ccall "console.h TCOD_console_set_window_title"
  tcod_console_set_window_title :: CString  -> IO()

-- Sets the root window title
setWindowTitle :: String -> IO ()
setWindowTitle s = (newCAString s) >>= tcod_console_set_window_title

foreign import ccall "console.h TCOD_console_is_window_closed"
  tcod_console_is_window_closed :: IO Bool

-- Returns true if the window is closed. The program should then exit
-- cleanly.
isWindowClosed :: IO Bool
isWindowClosed = tcod_console_is_window_closed

foreign import ccall "console.h TCOD_console_credits"
  tcod_console_credits :: IO ()

-- Prints out the tcod credits on the root console.
credits :: IO ()
credits = tcod_console_credits

foreign import ccall "console.h TCOD_console_credits_render"
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

foreign import ccall "console.h TCOD_console_credits_reset"
  tcod_console_credits_reset :: IO ()

-- Resets the credits that have been started by `creditsRender`
creditsReset :: IO()
creditsReset = tcod_console_credits_reset

foreign import ccall "console.h TCOD_console_set_default_background_ptr"
  tcod_console_set_default_background :: Ptr ()
                                         -> Ptr Color
                                         -> IO ()

-- Sets the default background colour of the console
setDefaultBackground :: Console -> Color -> IO ()
setDefaultBackground con col =
  alloca $ \colp ->
  withConsolePtr con $ \conp -> do
    poke colp col
    tcod_console_set_default_background conp colp

foreign import ccall "console.h TCOD_console_set_default_foreground_ptr"
  tcod_console_set_default_foreground :: Ptr ()
                                         -> Ptr Color
                                         -> IO ()

-- Sets the default background colour of the console
setDefaultForeground :: Console -> Color -> IO ()
setDefaultForeground con col =
  alloca $ \colp ->
  withConsolePtr con $ \conp -> do
    poke colp col
    tcod_console_set_default_foreground conp colp

foreign import ccall "console.h TCOD_console_clear"
  tcod_console_clear :: Ptr ()
                        -> IO ()

-- Clears the console
clear :: Console -> IO ()
clear con = withConsolePtr con $ \conp -> do
  tcod_console_clear conp

foreign import ccall "console.h TCOD_console_set_char_background_ptr"
  tcod_console_set_char_back :: Ptr ()
                                -> CInt
                                -> CInt
                                -> Ptr Color
                                -> CInt
                                -> IO ()

setCharBackground :: Console -> (Int, Int) -> Color -> BackgroundFlag -> IO ()
setCharBackground con (x, y) col bf =
  withConsolePtr con $ \conp ->
  alloca $ \colp -> do
    poke colp col
    tcod_console_set_char_back conp x' y' colp (unBF bf)
      where unBF (BackgroundFlag c) = c
            x' = CInt (fromIntegral x)
            y' = CInt (fromIntegral y)

foreign import ccall "console.h TCOD_console_set_char_foreground_ptr"
  tcod_console_set_char_fore :: Ptr ()
                                -> CInt
                                -> CInt
                                -> Ptr Color
                                -> IO ()

setCharForeground :: Console -> (Int, Int) -> Color -> IO ()
setCharForeground con (x, y) col =
  withConsolePtr con $ \conp ->
  alloca $ \colp -> do
    poke colp col
    tcod_console_set_char_fore conp x' y' colp
      where x' = CInt (fromIntegral x)
            y' = CInt (fromIntegral y)

foreign import ccall "console.h TCOD_console_set_char"
  tcod_console_set_char :: Ptr ()
                           -> CInt
                           -> CInt
                           -> CInt
                           -> IO ()

setChar :: Console -> (Int, Int) -> Char -> IO ()
setChar con (x, y) char = withConsolePtr con $ \conp -> do
  tcod_console_set_char conp x' y' char'
    where conv = CInt . fromIntegral
          x' = conv x
          y' = conv y
          char' = conv (ord char)

foreign import ccall "console.h TCOD_console_put_char"
  tcod_console_put_char :: Ptr ()
                           -> CInt
                           -> CInt
                           -> CInt
                           -> CInt
                           -> IO ()

putChar_ :: Console -> (Int, Int) -> Char -> BackgroundFlag -> IO ()
putChar_ con (x, y) char (BackgroundFlag bf) = withConsolePtr con $ \conp -> do
  tcod_console_put_char conp x' y' char' bf
  where conv = CInt . fromIntegral
        x' = conv x
        y' = conv y
        char' = conv (ord char)

foreign import ccall "console.h TCOD_console_put_char_ex_ptr"
  tcod_console_put_char_ex :: Ptr ()
                              -> CInt
                              -> CInt
                              -> CInt
                              -> Ptr Color
                              -> Ptr Color
                              -> IO ()

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

foreign import ccall "console.h TCOD_console_rect"
  tcod_console_rect :: Ptr ()
                       -> CInt
                       -> CInt
                       -> CInt
                       -> CInt
                       -> Bool
                       -> CInt
                       -> IO ()

rect :: Console -> (Int, Int) -> (Int, Int) -> Bool -> BackgroundFlag -> IO ()
rect con (x, y) (w, h) c (BackgroundFlag bf) = withConsolePtr con $ \conp -> do
  tcod_console_rect conp x' y' w' h' c bf
  where conv = CInt . fromIntegral
        x' = conv x
        y' = conv y
        w' = conv w
        h' = conv h

foreign import ccall "console.h TCOD_console_hline"
  tcod_console_hline :: Ptr ()
                        -> CInt
                        -> CInt
                        -> CInt
                        -> CInt
                        -> IO ()

hLine :: Console -> (Int, Int) -> Int -> BackgroundFlag -> IO ()
hLine con (x, y) l (BackgroundFlag bf) = withConsolePtr con $ \conp ->
  tcod_console_hline conp x' y' l' bf
  where conv = CInt . fromIntegral
        x' = conv x
        y' = conv y
        l' = conv l

foreign import ccall "console.h TCOD_console_vline"
  tcod_console_vline :: Ptr ()
                        -> CInt
                        -> CInt
                        -> CInt
                        -> CInt
                        -> IO ()

vLine :: Console -> (Int, Int) -> Int -> BackgroundFlag -> IO ()
vLine con (x, y) l (BackgroundFlag bf) = withConsolePtr con $ \conp ->
  tcod_console_vline conp x' y' l' bf
  where conv = CInt . fromIntegral
        x' = conv x
        y' = conv y
        l' = conv l

foreign import ccall "console.h TCOD_console_print_frame"
  tcod_console_print_frame :: Ptr ()
                              -> CInt
                              -> CInt
                              -> CInt
                              -> CInt
                              -> Bool
                              -> CInt
                              -> Ptr ()
                              -> IO ()

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

foreign import ccall unsafe "console.h TCOD_console_get_width"
  tcod_console_get_width :: Ptr ()
                            -> IO CInt

getWidth :: Console -> IO Int
getWidth con = fromIntegral `fmap` (withConsolePtr con tcod_console_get_width)

foreign import ccall unsafe "console.h TCOD_console_get_height"
  tcod_console_get_height :: Ptr ()
                            -> IO CInt

getHeight :: Console -> IO Int
getHeight con = fromIntegral `fmap` (withConsolePtr con tcod_console_get_height)

foreign import ccall unsafe "console.h TCOD_console_get_default_background_ptr"
  tcod_console_get_default_background :: Ptr ()
                                         -> Ptr Color
                                         -> IO ()

getDefaultBackground :: Console -> IO Color
getDefaultBackground con = withConsolePtr con $ \conp ->
                           alloca $ \colp -> do
                             tcod_console_get_default_background conp colp
                             peek colp

foreign import ccall unsafe "console.h TCOD_console_get_default_foreground_ptr"
  tcod_console_get_default_foreground :: Ptr ()
                                         -> Ptr Color
                                         -> IO ()

getDefaultForeground :: Console -> IO Color
getDefaultForeground con = withConsolePtr con $ \conp ->
                           alloca $ \colp -> do
                             tcod_console_get_default_foreground conp colp
                             peek colp

foreign import ccall unsafe "console.h TCOD_console_get_char_background_ptr"
  tcod_console_get_char_background :: Ptr ()
                                      -> CInt
                                      -> CInt
                                      -> Ptr Color
                                      -> IO ()

getCharBackground :: Console -> (Int, Int) -> IO Color
getCharBackground con (x, y) =
  withConsolePtr con $ \conp ->
  alloca $ \colp -> do
    tcod_console_get_char_background conp x' y' colp
    peek colp
    where conv = CInt . fromIntegral
          x' = conv x
          y' = conv y

foreign import ccall unsafe "console.h TCOD_console_get_char_foreground_ptr"
  tcod_console_get_char_foreground :: Ptr ()
                                      -> CInt
                                      -> CInt
                                      -> Ptr Color
                                      -> IO ()

getCharForeground :: Console -> (Int, Int) -> IO Color
getCharForeground con (x, y) =
  withConsolePtr con $ \conp ->
  alloca $ \colp -> do
    tcod_console_get_char_foreground conp x' y' colp
    peek colp
    where conv = CInt . fromIntegral
          x' = conv x
          y' = conv y

foreign import ccall unsafe "console.h TCOD_console_get_char"
  tcod_console_get_char :: Ptr ()
                           -> CInt
                           -> CInt
                           -> IO CInt

getChar_ :: Console -> (Int, Int) -> IO Char
getChar_ con (x, y) = withConsolePtr con $ \conp ->
                      tcod_console_get_char conp x' y' >>=
                     (return . chr . fromIntegral)
  where conv = CInt . fromIntegral
        x' = conv x
        y' = conv y

foreign import ccall unsafe "console.h TCOD_console_set_fade_ptr"
  tcod_console_set_fade :: CChar
                           -> Ptr Color
                           -> IO ()

setFade :: Int -> Color -> IO ()
setFade f c = alloca $ \cp -> do
  poke cp c
  tcod_console_set_fade (fromIntegral f) cp

foreign import ccall unsafe "console.h TCOD_console_get_fade"
  tcod_console_get_fade :: IO CChar

getFade :: IO Int
getFade = tcod_console_get_fade >>= (return . fromIntegral)

foreign import ccall unsafe "console.h TCOD_console_get_fading_color_ptr"
  tcod_console_get_fading_color :: Ptr Color
                                   -> IO ()

getFadingColor :: IO Color
getFadingColor = alloca $ \p -> do
  tcod_console_get_fading_color p
  peek p

foreign import ccall unsafe "console.h TCOD_console_flush"
  tcod_console_flush :: IO ()

flushConsole :: IO ()
flushConsole = tcod_console_flush

foreign import ccall unsafe "console.h TCOD_wait_for_keypress_ptr"
  tcod_console_wait_for_keypress :: Bool
                                    -> Ptr KeyEvent
                                    -> IO ()

waitForKeypress :: Bool -> IO KeyEvent
waitForKeypress f =
  alloca $ \kep ->
  tcod_console_wait_for_keypress f kep >>
  peek kep

foreign import ccall unsafe "console.h TCOD_console_new"
  tcod_console_new :: CInt
                      -> CInt
                      -> IO (Ptr ())

initOffscreen :: Int -> Int -> IO Console
initOffscreen w h = createConsole $ tcod_console_new w' h'
  where conv = CInt . fromIntegral
        w' = conv w
        h' = conv h

foreign import ccall unsafe "console.h TCOD_console_from_file"
  tcod_console_from_file :: CString
                            -> IO (Ptr ())

initOffscreenFromFile :: String -> IO Console
initOffscreenFromFile fname =
  newCAString fname >>=
  createConsole . tcod_console_from_file

foreign import ccall unsafe "console.h TCOD_console_load_asc"
  tcod_console_load_asc :: Ptr ()
                           -> CString
                           -> IO Bool

loadASC :: Console -> String -> IO Bool
loadASC RootConsole _ = return False
loadASC con fname =
  withConsolePtr con $ \conp ->
  newCAString fname >>=
  tcod_console_load_asc conp

foreign import ccall unsafe "console.h TCOD_console_load_afp"
  tcod_console_load_afp :: Ptr ()
                           -> CString
                           -> IO Bool

loadAFP :: Console -> String -> IO Bool
loadAFP RootConsole _ = return False
loadAFP con fname =
  withConsolePtr con $ \conp ->
  newCAString fname >>=
  tcod_console_load_afp conp

foreign import ccall unsafe "console.h TCOD_console_save_asc"
  tcod_console_save_asc :: Ptr ()
                           -> CString
                           -> IO Bool

saveASC :: Console -> String -> IO Bool
saveASC con fname =
  withConsolePtr con $ \conp ->
  newCAString fname >>=
  tcod_console_save_asc conp

foreign import ccall unsafe "console.h TCOD_console_save_afp"
  tcod_console_save_afp :: Ptr ()
                           -> CString
                           -> IO Bool

saveAFP :: Console -> String -> IO Bool
saveAFP con fname =
  withConsolePtr con $ \conp ->
  newCAString fname >>=
  tcod_console_save_afp conp

foreign import ccall unsafe "console.h TCOD_console_blit"
  tcod_console_blit :: Ptr () -> CInt -> CInt -> CInt -> CInt
                       -> Ptr () -> CInt -> CInt
                       -> CFloat -> CFloat
                       -> IO ()

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
        f = CFloat . fromRational
        fga' = f fga
        bga' = f bga

foreign import ccall unsafe "console.h TCOD_console_set_key_color_ptr"
  tcod_console_set_key_color :: Ptr ()
                                -> Ptr Color
                                -> IO ()

setKeyColor :: Console -> Color -> IO ()
setKeyColor con col =
  withCOnsolePtr con $ \conp ->
  alloca $ \colp ->
  tcod_console_set_key_color conp colp