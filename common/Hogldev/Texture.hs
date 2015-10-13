{-# LANGUAGE RecordWildCards #-}
module Hogldev.Texture (
    Texture(..)
  , textureBind
  , textureLoad
) where

import           Graphics.Rendering.OpenGL hiding (Texture)
import           Graphics.GLUtil (readTexture)

data Texture = Texture
               { textureObject :: !TextureObject
               , textureTarget :: !TextureTarget2D
               } deriving Show

textureBind :: Texture -> TextureUnit -> IO ()
textureBind Texture{..} textureUnit = do
    activeTexture $= textureUnit
    textureBinding textureTarget $= Just textureObject

textureLoad :: FilePath -> TextureTarget2D -> IO (Maybe Texture)
textureLoad fileName target = readTexture fileName >>= either fail bind
  where
    fail :: String -> IO (Maybe Texture)
    fail msg = do
        putStrLn ("Error loading texture '" ++ fileName ++ "': " ++ msg)
        return Nothing
    bind :: TextureObject -> IO (Maybe Texture)
    bind object = do
        textureBinding target $= Just object
        textureFilter  target $= ((Linear', Nothing), Linear')
        return (Just (Texture object target))
