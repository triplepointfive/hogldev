{-# LANGUAGE RecordWildCards #-}
module Hogldev.CubemapTexture (
    CubemapTexture (..)
  , CubeMapFilenames (..)
  , cubeMapTexBind
  , loadCubemapTexture
) where

import           Control.Arrow (second)
import           System.FilePath (combine)

import           Graphics.Rendering.OpenGL hiding (Texture)
import           Graphics.GLUtil (readTexture)

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

loadCubemapTexture :: CubeMapFilenames -> IO CubemapTexture
loadCubemapTexture CubeMapFilenames{..} = undefined -- readTexture fileName >>= either fail bind
  where
    fail :: FilePath -> String -> IO (Maybe CubemapTexture)
    fail fileName msg = do
        putStrLn ("Error loading texture '" ++ fileName ++ "': " ++ msg)
        return Nothing
    bind :: TextureObject -> IO (Maybe CubemapTexture)
    bind object = do
        textureBinding TextureCubeMap $= Just object
        textureFilter  TextureCubeMap $= ((Linear', Nothing), Linear')
        return (Just (CubemapTexture object))

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
