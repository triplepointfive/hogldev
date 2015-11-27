{-# LANGUAGE RecordWildCards #-}
module PickingTexture (
    PickingTexture (..)
  , PixelInfo(..)
  , initPickingTexture
  , enableWriting
  , disableWriting
  , readPixel
) where

import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Control.Monad (when)

import           Graphics.Rendering.OpenGL

data PickingTexture =
    PickingTexture
    { fbo            :: !FramebufferObject
    , pickingTexture :: !TextureObject
    , depthTexture   :: !TextureObject
    } deriving Show

data PixelInfo =
    PixelInfo
    { objectID :: !Float
    , drawID   :: !Float
    , primID   :: !Float
    } deriving Show

instance Storable PixelInfo where
    sizeOf ~(PixelInfo o d p) = sizeOf o + sizeOf d + sizeOf p
    alignment ~(PixelInfo o _ _) = alignment o
    peek ptr = do
        o <- peek (castPtr ptr)
        d <- peekByteOff (castPtr ptr) (sizeOf o)
        p <- peekByteOff (castPtr ptr) (sizeOf o + sizeOf d)
        return $ PixelInfo o d p
    poke ptr (PixelInfo o d p) = do
        poke (castPtr ptr) o
        pokeByteOff (castPtr ptr) (sizeOf o) d
        pokeByteOff (castPtr ptr) (sizeOf o + sizeOf d) p

initPickingTexture :: GLint -> GLint -> IO PickingTexture
initPickingTexture width height = do
    -- Create the FBO
    lFBO <- genObjectName
    bindFramebuffer Framebuffer $= lFBO

    -- Create the texture object for the primitive information buffer
    lPickingTexture <- genObjectName
    textureBinding Texture2D $= Just lPickingTexture
    texImage2D Texture2D NoProxy 0 RGB32F (TextureSize2D width height)
        0 (PixelData RGB Float nullPtr)
    framebufferTexture2D DrawFramebuffer (ColorAttachment 0) Texture2D lPickingTexture 0

    -- Create the texture object for the depth buffer
    lDepthTexture <- genObjectName
    textureBinding Texture2D $= Just lDepthTexture
    texImage2D Texture2D NoProxy 0 DepthComponent32f (TextureSize2D width height)
        0 (PixelData DepthComponent Float nullPtr)
    framebufferTexture2D DrawFramebuffer DepthAttachment Texture2D lDepthTexture 0

    readBuffer $= NoBuffers
    drawBuffer $= FBOColorAttachment 0

    -- Verify that the FBO is correct
    status <- framebufferStatus Framebuffer
    when (status /= Complete) $ putStrLn ("FB error, status: " ++ show status)

    -- Restore the default framebuffer
    textureBinding Texture2D $= Nothing
    bindFramebuffer Framebuffer $= defaultFramebufferObject

    return PickingTexture
        { fbo            = lFBO
        , pickingTexture = lPickingTexture
        , depthTexture   = lDepthTexture
        }

enableWriting :: PickingTexture -> IO ()
enableWriting PickingTexture{..} =
    bindFramebuffer DrawFramebuffer $= fbo

disableWriting :: PickingTexture -> IO ()
disableWriting PickingTexture{..} =
    bindFramebuffer DrawFramebuffer $= defaultFramebufferObject

readPixel :: PickingTexture -> GLint -> GLint -> IO PixelInfo
readPixel PickingTexture{..} x y = do
    bindFramebuffer ReadFramebuffer $= fbo
    readBuffer $= FBOColorAttachment 0

    pixelInfo <- withPtr $
        readPixels (Position x y) (Size 1 1) . PixelData RGB Float

    readBuffer $= NoBuffers
    bindFramebuffer ReadFramebuffer $= defaultFramebufferObject
    return pixelInfo

-- Stolen from inline-c library.
withPtr :: (Storable a) => (Ptr a -> IO b) -> IO a
withPtr f = alloca $ \ptr -> f ptr >> peek ptr
