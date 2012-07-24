{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module UI.TCOD.System
       ( -- * FPS functions
         setFPS
       , getFPS
       , getLastFrameLength

       -- * Precise time measurement
       , sleepMilli
       , elapsedMilli
       , elapsedSeconds

       -- * Screenshot
       , saveScreenshot

       -- * Fullscreen
       , forceFullscreenResolution
       , getCurrentResolution
       , getFullscreenOffsets

       -- * Font information
       , getCharSize
--       , updateChar

       -- * Renderer manipulation
       , setRenderer
       , getRenderer
       , Renderer(..)

       -- * Clipboard integration
       , setClipboard
       , getClipboard
       ) where

import UI.TCOD.Console.Types(Renderer(..))

import Foreign
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Foreign.C.String

foreign import ccall unsafe "sys.h TCOD_sys_set_fps"
  tcod_set_fps :: CInt
                  -> IO ()

-- | Limits the frames per second. The framerate should always be
--   limited, lest you use 100% CPU. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/system_time.html?c=true#0>
setFPS :: Int -> IO ()
setFPS = tcod_set_fps . CInt . fromIntegral

foreign import ccall unsafe "sys.h TCOD_sys_get_fps"
  tcod_get_fps :: IO CInt

-- | Returns the number of frames rendered in the last second. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/system_time.html?c=true#1>
getFPS :: IO Int
getFPS = fmap fromIntegral tcod_get_fps

foreign import ccall unsafe "sys.h TCOD_sys_get_last_frame_length"
  tcod_get_last_frame_length :: IO CFloat

-- | Returns the length in seconds of the last frame. Can be used to
--   update time depended objects. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/system_time.html?c=true#2>
getLastFrameLength :: IO Float
getLastFrameLength = fmap realToFrac tcod_get_last_frame_length

foreign import ccall unsafe "sys.h TCOD_sys_sleep_milli"
  tcod_sleep_milli :: CUInt
                      -> IO ()

-- | Blocks for the specified number of milliseconds. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/system_time.html?c=true#3>
sleepMilli :: Int -> IO ()
sleepMilli = tcod_sleep_milli . fromIntegral

foreign import ccall unsafe "sys.h TCOD_sys_elapsed_milli"
  tcod_elapsed_milli :: IO CUInt

-- | The number of milliseconds since the program started. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/system_time.html?c=true#4>
elapsedMilli :: IO Int
elapsedMilli = fmap fromIntegral tcod_elapsed_milli

foreign import ccall unsafe "sys.h TCOD_sys_elapsed_seconds"
  tcod_elapsed_seconds :: IO CFloat

-- | The number of seconds since the program has started. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/system_time.html?c=true#5>
elapsedSeconds :: IO Float
elapsedSeconds = fmap realToFrac tcod_elapsed_seconds

foreign import ccall unsafe "sys.h TCOD_sys_save_screenshot"
  tcod_save_screenshot :: CString
                          -> IO ()

-- | Saves a screenshot of the root console to the given file. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/system_screenshots.html?c=true#0>
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

-- | Forces the fullscreen resolution. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/system_misc.html?c=true#0>
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

-- | The current resolution. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/system_misc.html?c=true#1>
getCurrentResolution :: IO (Int, Int)
getCurrentResolution = twoPtrInputs tcod_get_current_resolution fromIntegral

foreign import ccall unsafe "sys.h TCOD_sys_get_fullscreen_offsets"
  tcod_get_fullscreen_offsets :: Ptr CInt
                                 -> Ptr CInt
                                 -> IO ()

-- | If the fullscreen resolution does not match the root console
--   size, black bars will appear by the side of the screen.
--   The fullscreen offset is the position in pixels of the top left
--   corner of the console. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/system_misc.html?c=true#2>
getFullscreenOffsets :: IO (Int, Int)
getFullscreenOffsets = twoPtrInputs tcod_get_fullscreen_offsets fromIntegral

foreign import ccall unsafe "sys.h TCOD_sys_get_char_size"
  tcod_get_char_size :: Ptr CInt
                        -> Ptr CInt
                        -> IO ()

-- | Returns the size of the font in pixels. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/system_misc.html?c=true#3>
getCharSize :: IO (Int, Int)
getCharSize = twoPtrInputs tcod_get_char_size fromIntegral

-- TODO: updateChar will be implemented when the image subsystem is
              -- implemented

foreign import ccall unsafe "sys.h TCOD_sys_set_renderer"
  tcod_set_renderer :: CInt -> IO ()

-- | Sets the internal renderer that libtcod uses. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/system_misc.html?c=true#5>
setRenderer :: Renderer -> IO ()
setRenderer (Renderer r) = tcod_set_renderer r

foreign import ccall unsafe "sys.h TCOD_sys_get_renderer"
  tcod_get_renderer :: IO CInt

-- | Returns the internal renderer that libtcod is using. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/system_misc.html?c=true#6>
getRenderer :: IO Renderer
getRenderer = Renderer `fmap` tcod_get_renderer

foreign import ccall unsafe "sys.h TCOD_sys_clipboard_set"
  tcod_clipboard_set :: CString
                        -> IO ()

-- | Copy data to the OS clipboard. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/system_clipboard.html?c=true#0>
setClipboard :: String -> IO ()
setClipboard s = newCAString s >>= tcod_clipboard_set

foreign import ccall unsafe "sys.h TCOD_sys_clipboard_get"
  tcod_clipboard_get :: IO CString

-- TODO: Free the CString
-- | Get the current contents of the OS clipboard. Wraps
--   <http://doryen.eptalys.net/data/libtcod/doc/1.5.1/html2/system_clipboard.html?c=true#1>
getClipboard :: IO String
getClipboard = do
  p <- tcod_clipboard_get
  s <- peekCString p
  free p
  return s