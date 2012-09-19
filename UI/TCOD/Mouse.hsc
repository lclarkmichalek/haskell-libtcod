{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module UI.TCOD.Mouse
       ( showCursor
       , isCursorVisible
       , moveMouse
       , MouseStatus(..)
       , mouseStatus
       ) where

import Foreign.C.Types
import Foreign.C.String

#include "cmouse.h"

{-|
   The mouse status struct. Wraps TCOD_mouse_t.
-}
data MouseStatus = MouseStatus
                   { mousePos :: (Int, Int)
                   , mouseDelta :: (Int, Int)
                   , mouseCell :: (Int, Int)
                   , mouseCellDelta :: (Int, Int)
                   , mouseLeft :: Bool
                   , mouseRight :: Bool
                   , mouseMiddle :: Bool
                   , mouseLeftP :: Bool
                   , mouseRightP :: Bool
                   , mouseMiddleP :: Bool
                   , mouseWheelUp :: Bool
                   , mouseWheelDown :: Bool
                   } deriving (Show, Eq)

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable MouseStatus where
  sizeOf _ = #{size TCOD_mouse_t}
  alignment _ = #{alignment TCOD_mouse_t}
  peek ptr = do
    x <- #{peek TCOD_mouse_t, x} ptr
    y <- #{peek TCOD_mouse_t, y} ptr
    dx <- #{peek TCOD_mouse_t, dx} ptr
    dy <- #{peek TCOD_mouse_t, dy} ptr
    cx <- #{peek TCOD_mouse_t, cx} ptr
    cy <- #{peek TCOD_mouse_t, cy} ptr
    dcx <- #{peek TCOD_mouse_t, dcx} ptr
    dcy <- #{peek TCOD_mouse_t, dcy} ptr
    l <- #{peek TCOD_mouse_t, lbutton} ptr
    r <- #{peek TCOD_mouse_t, rbutton} ptr
    m <- #{peek TCOD_mouse_t, mbutton} ptr
    lp <- #{peek TCOD_mouse_t, lbutton_pressed} ptr
    rp <- #{peek TCOD_mouse_t, rbutton_pressed} ptr
    mp <- #{peek TCOD_mouse_t, mbutton_pressed} ptr
    wup <- #{peek TCOD_mouse_t, wheel_up} ptr
    wdn <- #{peek TCOD_mouse_t, wheel_down} ptr
    return $ MouseStatus (x, y) (dx, dy) (cx, cy) (dcx, dcy) l r m lp rp mp wup wdn
  poke ptr (MouseSatus (x, y) (dx, dy) (cx, cy) (dcx, dcy) l r m lp rp mp wup wdn) =
    do #{poke TCOD_mouse_t, x} ptr x
       #{poke TCOD_mouse_t, y} ptr y
       #{poke TCOD_mouse_t, dx} ptr dx
       #{poke TCOD_mouse_t, dy} ptr dy
       #{poke TCOD_mouse_t, cx} ptr cx
       #{poke TCOD_mouse_t, cy} ptr cy
       #{poke TCOD_mouse_t, dcx} ptr dcx
       #{poke TCOD_mouse_t, dcy} ptr dcy
       #{poke TCOD_mouse_t, lbutton} ptr l
       #{poke TCOD_mouse_t, rbutton} ptr r
       #{poke TCOD_mouse_t, mbutton} ptr m
       #{poke TCOD_mouse_t, lbutton_pressed} ptr lp
       #{poke TCOD_mouse_t, rbutton_pressed} ptr rp
       #{poke TCOD_mouse_t, mbutton_pressed} ptr rp
       #{poke TCOD_mouse_t, wheel_up} ptr wup
       #{poke TCOD_mouse_t, wheel_down} ptr wdn

boolToNum :: Num a => Bool -> a
boolToNum True = 1
boolToNum False = 0

numToBool :: Num a => a -> Bool
numToBool 0 = False
numToBool _ = True

foreign import ccall unsafe "cmouse.h TCOD_mouse_show_cursor"
  show_cursor :: CUChar -> IO ()

-- | Show or hide the mouse cursor.
showCursor :: Bool -> IO ()
showCursor = show_cursor . boolToNum

foreign import ccall unsafe "cmouse.h TCOD_mouse_is_cursor_visible"
  is_cursor_visible :: IO CUChar

-- | Returns the status of the cursor.
isCursorVisible :: IO Bool
isCursorVisible = numToBool . is_cursor_visible

foreign import ccall unsafe "cmouse.h TCOD_mouse_move"
  mouse_move :: CInt -> CInt -> IO ()

moveMouse :: (Int, Int) -> IO ()
moveMouse (x, y) =
  move_mouse (fromIntegral x) (fromIntegral y)

foreign import ccall unsafe "cmouse.h TCOD_mouse_get_status_ptr"
  mouse_status :: Ptr MouseStatus -> IO ()

-- | Gets the current mouse status
mouseStatus :: IO MouseStatus
mouseStatus = alloca $ \msp ->
  mouse_status msp
  peek msp