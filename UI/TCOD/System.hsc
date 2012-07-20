{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module UI.TCOD.System
       ( setFPS
       , getFPS
       , getLastFrameLength

       , sleepMilli
       , elapsedMilli
       , elapsedSeconds

       , saveScreenshot

       , forceFullscreenResolution
       , getCurrentResolution
       , getFullscreenOffsets

       , getCharSize
--       , updateChar

       , setRenderer
       , getRenderer
       , Renderer(..)

       , setClipboard
       , getClipboard
       ) where

import UI.TCOD.Console.Types(Renderer(..))

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

foreign import ccall unsafe "sys.h TCOD_sys_set_fps"
  tcod_set_fps :: CInt
                  -> IO ()

setFPS :: Int -> IO ()
setFPS = tcod_set_fps . CInt . fromIntegral

foreign import ccall unsafe "sys.h TCOD_sys_get_fps"
  tcod_get_fps :: IO CInt

getFPS :: IO Int
getFPS = fmap fromIntegral tcod_get_fps

foreign import ccall unsafe "sys.h TCOD_sys_get_last_frame_length"
  tcod_get_last_frame_length :: IO CFloat

getLastFrameLength :: IO Float
getLastFrameLength = fmap realToFrac tcod_get_last_frame_length

foreign import ccall unsafe "sys.h TCOD_sys_sleep_milli"
  tcod_sleep_milli :: CUInt
                      -> IO ()

sleepMilli :: Int -> IO ()
sleepMilli = tcod_sleep_milli . fromIntegral

foreign import ccall unsafe "sys.h TCOD_sys_elapsed_milli"
  tcod_elapsed_milli :: IO CUInt

elapsedMilli :: IO Int
elapsedMilli = fmap fromIntegral tcod_elapsed_milli

foreign import ccall unsafe "sys.h TCOD_sys_elapsed_seconds"
  tcod_elapsed_seconds :: IO CFloat

elapsedSeconds :: IO Float
elapsedSeconds = fmap realToFrac tcod_elapsed_seconds

foreign import ccall unsafe "sys.h TCOD_sys_save_screenshot"
  tcod_save_screenshot :: CString
                          -> IO ()

saveScreenshot :: String -> IO ()
saveScreenshot s = newCAString s >>= tcod_save_screenshot

-- TODO:
-- Not implementing all the filesystem stuff as haskell already
-- implements it. Also not implementing SDL rendering, as that would
-- require importing SDL and shizzle

foreign import ccall unsafe "sys.h TCOD_sys_force_fullscreen_resolution"
  tcod_force_fullscreen_resolution :: CInt
                                      -> CInt
                                      -> IO ()

forceFullscreenResolution :: Int -> Int -> IO ()
forceFullscreenResolution w h = tcod_force_fullscreen_resolution w' h'
  where w' = fromIntegral w
        h' = fromIntegral h

twoPtrInputs :: Storable a => (Ptr a -> Ptr a -> IO ()) -> (a -> b) -> IO (b, b)
twoPtrInputs f c =
  alloca $ \p1 ->
  alloca $ \p2 -> do
    f p1 p2
    v1 <- peek p1
    v2 <- peek p2
    return (c v1, c v2)

foreign import ccall unsafe "sys.h TCOD_sys_get_current_resolution"
  tcod_get_current_resolution :: Ptr CInt
                                 -> Ptr CInt
                                 -> IO ()

getCurrentResolution :: IO (Int, Int)
getCurrentResolution = twoPtrInputs tcod_get_current_resolution fromIntegral

foreign import ccall unsafe "sys.h TCOD_sys_get_fullscreen_offsets"
  tcod_get_fullscreen_offsets :: Ptr CInt
                                 -> Ptr CInt
                                 -> IO ()

getFullscreenOffsets :: IO (Int, Int)
getFullscreenOffsets = twoPtrInputs tcod_get_fullscreen_offsets fromIntegral

foreign import ccall unsafe "sys.h TCOD_sys_get_char_size"
  tcod_get_char_size :: Ptr CInt
                        -> Ptr CInt
                        -> IO ()

getCharSize :: IO (Int, Int)
getCharSize = twoPtrInputs tcod_get_char_size fromIntegral

-- TODO: updateChar will be implemented when the image subsystem is
              -- implemented

foreign import ccall unsafe "sys.h TCOD_sys_set_renderer"
  tcod_set_renderer :: CInt -> IO ()

setRenderer :: Renderer -> IO ()
setRenderer (Renderer r) = tcod_set_renderer r

foreign import ccall unsafe "sys.h TCOD_sys_get_renderer"
  tcod_get_renderer :: IO CInt

getRenderer :: IO Renderer
getRenderer = Renderer `fmap` tcod_get_renderer

foreign import ccall unsafe "sys.h TCOD_sys_clipboard_set"
  tcod_clipboard_set :: CString
                        -> IO ()

setClipboard :: String -> IO ()
setClipboard s = newCAString s >>= tcod_clipboard_set

foreign import ccall unsafe "sys.h TCOD_sys_clipboard_get"
  tcod_clipboard_get :: IO CString

-- TODO: Free the CString
getClipboard :: IO String
getClipboard = tcod_clipboard_get >>= peekCString