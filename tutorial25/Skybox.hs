{-# LANGUAGE RecordWildCards #-}
module Skybox (
    Skybox (..)
  , initSkybox
  , skyboxRender
) where

import           Graphics.Rendering.OpenGL

import           Hogldev.Pipeline (
                    Pipeline(..), getTrans,
                    PersProj(..)
                 )
import           Hogldev.Camera (Camera(..))
import           Hogldev.CubemapTexture

import           Mesh
import           SkyboxTechnique

data Skybox =
    Skybox
    { technique   :: !SkyboxTechnique
    , cubeMapTex  :: !CubemapTexture
    , mesh        :: !Mesh
    , skyPersProj :: !PersProj
    } deriving (Show)

initSkybox :: PersProj -> CubeMapFilenames -> IO Skybox
initSkybox perspective cubeMapFiles = do
    tech <- initSkyboxTechnique
    enableSkyboxTechnique tech
    setSkyboxTechniqueUnit tech 0

    cubeMapTexture <- loadCubemapTexture cubeMapFiles

    sphereMesh <- loadMesh "assets/sphere.obj"
    return Skybox
        { technique   = tech
        , cubeMapTex  = cubeMapTexture
        , mesh        = sphereMesh
        , skyPersProj = perspective
        }

skyboxRender :: Skybox -> Camera -> IO ()
skyboxRender Skybox{..} camera = do
    enableSkyboxTechnique technique

    oldCullFaceMode <- get cullFace
    oldDepthMode <- get depthFunc

    cullFace $= Just Front
    depthFunc $= Just Lequal

    setSkyboxTechniqueWVP technique $ getTrans
        WVPPipeline {
            worldInfo  = cameraPos camera,
            scaleInfo  = Vector3 20.0 20.0 20.0,
            rotateInfo = Vector3 0 0 0,
            persProj   = skyPersProj,
            pipeCamera = camera
        }
    cubeMapTexBind cubeMapTex (TextureUnit 0)
    renderMesh mesh

    cullFace $= oldCullFaceMode
    depthFunc $= oldDepthMode
