{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module UI.TCOD.Image
       ( Image(..)
       , ImageStruct

       , newImage
       , imageFromFile
       , imageFromConsole

       , getSize
       , getPixel
       , getAlpha
       , isTransparent
       , getMipMap

       , clearImage
       , putPixel
       , scaleImage
       , hflipImage
       , vflipImage
       , rotateImage90
       , invertImage

       , saveImage

       , blitImageRect
       , blitImage
       , blitImage2x
       , setKeyColor
       ) where

import UI.TCOD.Console (ConsoleStruct(..), Console(..))
import UI.TCOD.Console.Types (BackgroundFlag(..))
import UI.TCOD.Color

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Unsafe
import Foreign.ForeignPtr.Safe

data ImageStruct = ImageStruct
data Image = Image (ForeignPtr ImageStruct)

foreign import ccall unsafe "cimage.h &TCOD_image_delete"
  image_delete :: FunPtr (Ptr ImageStruct -> IO ())

makeImage :: Ptr ImageStruct -> IO Image
makeImage imgp = fmap Image $ newForeignPtr image_delete imgp

ci = CInt . fromIntegral
cf = CFloat

foreign import ccall unsafe "cimage.h TCOD_image_new"
  new_image :: CInt ->
               CInt ->
               IO (Ptr ImageStruct)

newImage :: Int -> Int -> IO Image
newImage w h = new_image w' h' >>= makeImage
  where w' = ci w
        h' = ci h

foreign import ccall unsafe "cimage.h TCOD_image_load"
  load_image :: CString -> IO (Ptr ImageStruct)

imageFromFile :: String -> IO (Maybe Image)
imageFromFile name =
  withCAString name $ \strp -> do
    imgp <- load_image strp
    if imgp == nullPtr
      then return Nothing
      else Just `fmap` makeImage imgp

foreign import ccall unsafe "cimage.h TCOD_image_from_console"
  image_from_console :: Ptr ConsoleStruct ->
                        IO (Ptr ImageStruct)

imageFromConsole :: Console -> IO Image
imageFromConsole (Console con) =
  withForeignPtr con $ \conp ->
  image_from_console conp >>= makeImage

foreign import ccall unsafe "cimage.h TCOD_image_get_size"
  image_get_size :: Ptr ImageStruct ->
                    Ptr CInt ->
                    Ptr CInt ->
                    IO ()

getSize :: Image -> IO (Int, Int)
getSize (Image fp) =
  withForeignPtr fp $ \imgp ->
  alloca $ \xp ->
  alloca $ \yp -> do
    image_get_size imgp xp yp
    x <- peek xp
    y <- peek yp
    return (fromIntegral x, fromIntegral y)

foreign import ccall unsafe "cimage.h TCOD_image_get_pixel_ptr"
  image_get_pixel :: Ptr ImageStruct ->
                     CInt -> CInt ->
                     Ptr Color ->
                     IO ()

getPixel :: Image -> (Int, Int) -> IO Color
getPixel (Image fp) (x, y) =
  withForeignPtr fp $ \imgp ->
  alloca $ \colp -> do
    image_get_pixel imgp x' y' colp
    peek colp
  where x' = ci x
        y' = ci y

foreign import ccall unsafe "cimage.h TCOD_image_get_alpha"
  image_get_alpha :: Ptr ImageStruct ->
                     CInt -> CInt ->
                     IO CInt

getAlpha :: Image -> (Int, Int) -> IO Int
getAlpha (Image fp) (x, y) =
  withForeignPtr fp $ \imgp ->
  fromIntegral `fmap`
  image_get_alpha imgp x' y'
  where x' = ci x
        y' = ci y

isTransparent :: Image -> (Int, Int) -> IO Bool
isTransparent img coord = fmap (== 0) $ (getAlpha img coord)

foreign import ccall unsafe "cimage.h TCOD_image_get_mipmap_ptr"
  image_get_mipmap :: Ptr ImageStruct ->
                      CFloat -> CFloat ->
                      CFloat -> CFloat ->
                      Ptr Color ->
                      IO ()

getMipMap :: Image -> (Float, Float) -> (Float, Float) -> IO Color
getMipMap (Image fp) (x0, y0) (x1, y1) =
  withForeignPtr fp $ \imgp ->
  alloca $ \colp ->
  image_get_mipmap imgp x0' y0' x1' y1' colp >>
  peek colp
  where x0' = cf x0
        y0' = cf y0
        x1' = cf x1
        y1' = cf y1

foreign import ccall unsafe "cimage.h TCOD_image_clear_ptr"
  image_clear :: Ptr ImageStruct ->
                 Ptr Color ->
                 IO ()

clearImage :: Image -> Color -> IO ()
clearImage (Image fp) col =
  withForeignPtr fp $ \imgp ->
  alloca $ \colp ->
  poke colp col >>
  image_clear imgp colp

foreign import ccall unsafe "cimage.h TCOD_image_put_pixel_ptr"
  image_put_pixel :: Ptr ImageStruct ->
                     CInt -> CInt ->
                     Ptr Color ->
                     IO ()

putPixel :: Image -> (Int, Int) -> Color -> IO ()
putPixel (Image fp) (x, y) col =
  withForeignPtr fp $ \imgp ->
  alloca $ \colp ->
  poke colp col >>
  image_put_pixel imgp x' y' colp
  where x' = ci x
        y' = ci y

foreign import ccall unsafe "cimage.h TCOD_image_scale"
  image_scale :: Ptr ImageStruct ->
                 CInt -> CInt ->
                 IO ()

scaleImage :: Image -> (Int, Int) -> IO ()
scaleImage (Image fp) (x, y) =
  withForeignPtr fp $ \imgp ->
  image_scale imgp (ci x) (ci y)

foreign import ccall unsafe "cimage.h TCOD_image_hflip"
  image_hflip :: Ptr ImageStruct -> IO ()

hflipImage :: Image -> IO ()
hflipImage (Image fp) = withForeignPtr fp image_hflip

foreign import ccall unsafe "cimage.h TCOD_image_vflip"
  image_vflip :: Ptr ImageStruct -> IO ()

vflipImage :: Image -> IO ()
vflipImage (Image fp) = withForeignPtr fp image_vflip

foreign import ccall unsafe "cimage.h TCOD_image_rotate90"
  image_rotate :: Ptr ImageStruct -> CInt -> IO ()

rotateImage90 :: Image -> Int -> IO ()
rotateImage90 img n
  | n `mod` 4 == 0 = return ()
  | otherwise      = rotate' img (n `mod` 4)
  where rotate' (Image fp) n' =
          withForeignPtr fp $ \imgp ->
          image_rotate imgp (ci n')

foreign import ccall unsafe "cimage.h TCOD_image_invert"
  image_invert :: Ptr ImageStruct -> IO ()

invertImage :: Image -> IO ()
invertImage (Image fp) = withForeignPtr fp image_invert

foreign import ccall unsafe "cimage.h TCOD_image_save"
  image_save :: Ptr ImageStruct -> CString -> IO ()

saveImage :: Image -> String -> IO ()
saveImage (Image fp) str =
  withCAString str $ \cstr ->
  withForeignPtr fp $ \imgp ->
  image_save imgp cstr

foreign import ccall unsafe "cimage.h TCOD_image_blit_rect"
  blit_rect :: Ptr ImageStruct ->
               Ptr ConsoleStruct ->
               CInt -> CInt ->
               CInt -> CInt ->
               CInt ->
               IO ()

blitImageRect :: Image -> Console ->
                 (Int, Int) -> (Int, Int) ->
                 BackgroundFlag -> IO ()
blitImageRect (Image ifp) (Console cfp) (x, y) (w, h) (BackgroundFlag bf) =
  withForeignPtr ifp $ \imgp ->
  withForeignPtr cfp $ \conp ->
  blit_rect imgp conp x' y' w' h' bf
  where x' = ci x
        y' = ci y
        w' = ci w
        h' = ci h

foreign import ccall unsafe "cimage.h TCOD_image_blit"
  blit :: Ptr ImageStruct ->
          Ptr ConsoleStruct ->
          CInt -> CInt ->
          CInt ->
          CFloat -> CFloat -> CFloat ->
          IO ()

blitImage :: Image -> Console -> (Int, Int) -> BackgroundFlag ->
             (Float, Float) -> Float -> IO ()
blitImage (Image ifp) (Console cfp) (x, y) (BackgroundFlag bf) (sx, sy) a =
  withForeignPtr ifp $ \imgp ->
  withForeignPtr cfp $ \conp ->
  blit imgp conp x' y' bf sx' sy' a'
  where x' = ci x
        y' = ci y
        sx' = cf sx
        sy' = cf sy
        a' = cf a

foreign import ccall unsafe "cimage.h TCOD_image_set_key_color_ptr"
  set_key_color :: Ptr ImageStruct -> Ptr Color -> IO ()

setKeyColor :: Image -> Color -> IO ()
setKeyColor (Image fp) c =
  withForeignPtr fp $ \imgp ->
  alloca $ \colp ->
  poke colp c >>
  set_key_color imgp colp

foreign import ccall unsafe "cimage.h TCOD_image_blit_2x"
  blit_2x :: Ptr ImageStruct ->
             Ptr ConsoleStruct ->
             CInt -> CInt ->
             CInt -> CInt ->
             CInt -> CInt ->
             IO ()

blitImage2x :: Image -> Console -> (Int, Int) -> (Int, Int) ->
               (Int, Int) -> IO()
blitImage2x (Image ifp) (Console cfp) (dx, dy) (sx, sy) (w, h) =
  withForeignPtr ifp $ \imgp ->
  withForeignPtr cfp $ \conp ->
  blit_2x imgp conp dx' dy' sx' sy' w' h'
  where dx' = ci dx
        dy' = ci dy
        sx' = ci sx
        sy' = ci sy
        w' = ci w
        h' = ci h