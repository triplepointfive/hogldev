{-# LANGUAGE RecordWildCards #-}
module Skybox (
    Skybox (..)
  , initSkybox
) where

import           Data.IORef

import           Graphics.Rendering.OpenGL

import           Hogldev.Pipeline (
                    Pipeline(..), getTrans,
                    PersProj(..)
                 )
import           Hogldev.Camera (Camera(..))

import           SkyboxTechnique

data Skybox =
    Skybox
    { technique  :: !SkyboxTechnique
    , cameraRef  :: !(IORef Camera)
    , cubeMapTex :: !TextureCubeMap
    , mesh       :: !Mesh
    , persProj   :: !PersProj
    } deriving (Show)

initSkybox :: IORef Camera -> PersProj -> IO Skybox
initSkybox = undefined

skyboxRender :: Skybox -> IO ()
skyboxRender Skybox{..} = do
    camera <- readIORef cameraRef

    enableSkyboxTechnique technique

    oldCullFaceMode <- get cullFace
    oldDepthMode <- get depthFunc

    cullFace $= Front
    depthFunc $= Lequal

    setSkyboxTechniqueWVP technique $ getTrans
        WVPPipeline {
            worldInfo  = cameraPos camera,
            scaleInfo  = Vector3 20.0 20.0 20.0,
            rotateInfo = Vector3 0 0 0,
            persProj   = persProj,
            pipeCamera = camera
        }
    cubeMapTexBind cubeMapTex (TextureUnit 0)
    renderMesh mesh

    cullFace $= oldCullFaceMode
    depthFunc $= oldDepthMode
