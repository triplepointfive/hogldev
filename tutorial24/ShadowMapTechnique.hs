{-# LANGUAGE RecordWildCards #-}
module ShadowMapTechnique (
    ShadowMapTechnique(..)
  , initShadowMapTechnique
  , setShadowMapWVP
  , setShadowMapTextureUnit
  , enableShadowMapTechnique
) where

import           Graphics.GLUtil
import           Graphics.Rendering.OpenGL

import           Hogldev.Technique

data ShadowMapTechnique =
    ShadowMapTechnique
    { lProgram    :: !Program
    , lWVPLoc     :: !UniformLocation
    , lSamplerLoc :: !UniformLocation
    }

initShadowMapTechnique :: IO ShadowMapTechnique
initShadowMapTechnique = do
    program <- createProgram
    addShader program "tutorial23/shadow_map.vs" VertexShader
    addShader program "tutorial23/shadow_map.fs" FragmentShader
    finalize program

    wvpLoc <- getUniformLocation program "gWVP"
    samplerLoc <- getUniformLocation program "gShadowMap"

    return ShadowMapTechnique
        { lProgram                          = program
        , lWVPLoc                           = wvpLoc
        , lSamplerLoc                       = samplerLoc
        }

setShadowMapWVP :: ShadowMapTechnique -> [[GLfloat]] -> IO ()
setShadowMapWVP ShadowMapTechnique{..} mat = uniformMat lWVPLoc $= mat

setShadowMapTextureUnit :: ShadowMapTechnique -> GLuint -> IO ()
setShadowMapTextureUnit ShadowMapTechnique{..} textureUnit =
    uniformScalar lSamplerLoc $= textureUnit

enableShadowMapTechnique :: ShadowMapTechnique -> IO ()
enableShadowMapTechnique ShadowMapTechnique{..} = enableTechnique lProgram
