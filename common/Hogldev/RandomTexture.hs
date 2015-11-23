module Hogldev.RandomTexture (
    RandomTexture(..)
  , textureBind
  , textureLoad
) where

import Control.Monad (replicateM)
import Foreign.Marshal.Array (withArray)
import System.Random

import Graphics.Rendering.OpenGL

newtype RandomTexture = RandomTexture TextureObject

textureBind :: RandomTexture -> TextureUnit -> IO ()
textureBind (RandomTexture textureObject) textureUnit = do
    activeTexture $= textureUnit
    textureBinding Texture2D $= Just textureObject

textureLoad :: GLint -> IO RandomTexture
textureLoad size = do
    texture <- genObjectName
    textureBinding Texture2D $= Just texture

    textureData <- replicateM (fromIntegral textureSize)
        (randomRIO (0, 1)) :: IO [GLfloat]

    withArray textureData $ \ptr ->
        texImage2D Texture2D NoProxy 0 RGB32F (TextureSize2D size size) 0
            (PixelData RGB Float ptr)

    textureFilter  Texture2D $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, Repeat)
    textureWrapMode Texture2D T $= (Repeated, Repeat)

    textureBinding Texture2D $= Nothing

    return (RandomTexture texture)
  where
    textureSize = size * size * 3
