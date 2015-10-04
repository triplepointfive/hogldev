module ShadowMapFBO (
    initializeShadowMapFBO
  , bindForWriting
  , bindForReading
) where

import Control.Monad (when)
import GHC.Ptr (nullPtr)

import Graphics.Rendering.OpenGL

-- I'm too lazy to init Utils.hs.
windowWidth, windowHeight :: GLsizei
windowWidth = 1024
windowHeight = 768

data ShadowMapFBO = ShadowMapFBO FramebufferObject TextureObject
    deriving (Show, Eq)

initializeShadowMapFBO :: IO ShadowMapFBO
initializeShadowMapFBO = do
    -- Create the FBO.
    fbo <- genObjectName
    -- Create the depth buffer.
    shadowMap <- genObjectName
    textureBinding  Texture2D $= Just shadowMap
    texImage2D Texture2D NoProxy 0 DepthComponent' screenSize 0 pixelData
    textureFilter   Texture2D $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

    bindFramebuffer Framebuffer $= fbo
    framebufferTexture2D DrawFramebuffer DepthAttachment Texture2D shadowMap 0

    -- Disable writes to the color buffer.
    drawBuffer $= NoBuffers
    readBuffer $= NoBuffers

    status <- framebufferStatus Framebuffer
    when (Complete /= status) $
        putStrLn ("FB error, status: " ++ show status)

    return (ShadowMapFBO fbo shadowMap)
  where
    screenSize = TextureSize2D windowWidth windowHeight
    pixelData  = PixelData DepthComponent Float nullPtr

bindForWriting :: ShadowMapFBO -> IO ()
bindForWriting (ShadowMapFBO fbo _) = bindFramebuffer DrawFramebuffer $= fbo

bindForReading :: ShadowMapFBO -> TextureUnit -> IO ()
bindForReading (ShadowMapFBO _ shadowMap) textureUnit = do
    activeTexture $= textureUnit
    textureBinding Texture2D $= Just shadowMap
