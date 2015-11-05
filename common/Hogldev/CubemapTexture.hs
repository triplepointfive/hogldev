{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
module Hogldev.CubemapTexture (
    CubemapTexture (..)
  , CubeMapFilenames (..)
  , cubeMapTexBind
  , loadCubemapTexture
) where

import           Control.Arrow (second)
import           Control.Monad (forM_)
import           System.FilePath (combine)

import qualified Graphics.Rendering.OpenGL.GL.VertexArrays as GL
import           Graphics.Rendering.OpenGL hiding (Texture)
import           Graphics.GLUtil.JuicyTextures
import           Graphics.GLUtil

data CubemapTexture =
    CubemapTexture
    { textureObject :: !TextureObject
    } deriving Show

data CubeMapFilenames =
    CubeMapFilenames
    { directory    :: !FilePath
    , posXFilename :: !FilePath
    , negXFilename :: !FilePath
    , posYFilename :: !FilePath
    , negYFilename :: !FilePath
    , posZFilename :: !FilePath
    , negZFilename :: !FilePath
    } deriving Show

-- | A helper function to handle images. Stolen from GLUtil library.
loadCubeTexture :: forall a. IsPixelData a =>
                 TextureTargetCubeMapFace -> TextureObject -> TexInfo a -> IO TextureObject
loadCubeTexture target obj tex = do
    textureBinding Texture2D $= Just obj
    loadTex $ texColor tex
    return obj
  where
    loadTex TexMono = case pixelType of
        GL.UnsignedShort -> loadAux Luminance16 Luminance
        GL.Float         -> loadAux R32F Red
        GL.HalfFloat     -> loadAux R16F Red
        GL.UnsignedByte  -> loadAux R8 Red
        _                -> loadAux Luminance' Luminance
    loadTex TexRG = case pixelType of
        GL.UnsignedShort -> loadAux RG16 RGInteger
        GL.Float -> loadAux RG32F RG
        GL.HalfFloat -> loadAux RG16F RG
        GL.UnsignedByte -> loadAux RG8UI RGInteger
        GL.Byte -> loadAux RG8I RGInteger
        GL.Int -> loadAux RG32I RGInteger
        GL.UnsignedInt -> loadAux RG32UI RGInteger
        _ -> error "Unknown pixelType for TexRG"
    loadTex TexRGB = loadAux RGBA' RGB
    loadTex TexBGR = loadAux RGBA' BGR
    loadTex TexRGBA = loadAux RGBA' RGBA
    sz = TextureSize2D (texWidth tex) (texHeight tex)
    pixelType = glType (undefined::Elem a)
    loadAux i e = withPixels (texData tex)
        (texImage2D target NoProxy 0 i sz 0 . PixelData e pixelType)

loadCubemapTexture :: CubeMapFilenames -> IO CubemapTexture
loadCubemapTexture CubeMapFilenames{..} = do
    [object] <- genObjectNames 1
    textureBinding TextureCubeMap $= Just object

    forM_ targetFile $ \ (target, fileName) -> do
        res <- readTexInfo fileName (loadCubeTexture target object)
        case res of
            Left msg -> error msg
            Right _  -> do
                textureFilter   TextureCubeMap   $= ((Linear', Nothing), Linear')
                textureWrapMode TextureCubeMap S $= (Repeated, ClampToEdge)
                textureWrapMode TextureCubeMap T $= (Repeated, ClampToEdge)
                textureWrapMode TextureCubeMap R $= (Repeated, ClampToEdge)

    return CubemapTexture { textureObject = object }
  where
    targetFile :: [(TextureTargetCubeMapFace, FilePath)]
    targetFile = map (second (combine directory))
      [ (TextureCubeMapPositiveX, posXFilename)
      , (TextureCubeMapNegativeX, negXFilename)
      , (TextureCubeMapPositiveY, posYFilename)
      , (TextureCubeMapNegativeY, negYFilename)
      , (TextureCubeMapPositiveZ, posZFilename)
      , (TextureCubeMapNegativeZ, negZFilename)
      ]

cubeMapTexBind :: CubemapTexture -> TextureUnit -> IO ()
cubeMapTexBind CubemapTexture{..} textureUnit = do
    activeTexture $= textureUnit
    textureBinding TextureCubeMap $= Just textureObject
