{-# LANGUAGE RecordWildCards #-}
module SkyboxTechnique (
    SkyboxTechnique (..)
  , initSkyboxTechnique
  , setSkyboxTechniqueWVP
  , setSkyboxTechniqueUnit
  , enableSkyboxTechnique
) where

import           Graphics.GLUtil
import           Graphics.Rendering.OpenGL

import           Hogldev.Technique

data SkyboxTechnique =
    SkyboxTechnique
    { lProgram    :: !Program
    , lWVPLoc     :: !UniformLocation
    , lTextureLoc :: !UniformLocation
    } deriving Show

initSkyboxTechnique :: IO SkyboxTechnique
initSkyboxTechnique = do
    program <- createProgram
    addShader program "tutorial25/skybox.vs" VertexShader
    addShader program "tutorial25/skybox.fs" FragmentShader
    finalize program

    wvpLoc <- getUniformLocation program "gWVP"
    textureLoc <- getUniformLocation program "gCubemapTexture;"

    return SkyboxTechnique
        { lProgram     = program
        , lWVPLoc      = wvpLoc
        , lTextureLoc  = textureLoc
        }

setSkyboxTechniqueWVP :: SkyboxTechnique -> [[GLfloat]] -> IO ()
setSkyboxTechniqueWVP SkyboxTechnique{..} mat = uniformMat lWVPLoc $= mat

setSkyboxTechniqueUnit :: SkyboxTechnique -> GLint -> IO ()
setSkyboxTechniqueUnit SkyboxTechnique{..} textureUnit =
    uniformScalar lTextureLoc $= textureUnit

enableSkyboxTechnique :: SkyboxTechnique -> IO ()
enableSkyboxTechnique SkyboxTechnique{..} = enableTechnique lProgram
