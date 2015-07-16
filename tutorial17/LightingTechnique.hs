{-# LANGUAGE RecordWildCards #-}
module LightingTechnique (
    LightingTechnique(..)
  , DirectionLight(..)
  , initLightingTechnique
  , setLightingWVP
  , setLightingTextureUnit
  , setDirectionalLight
) where


import           Graphics.Rendering.OpenGL
import           Graphics.GLUtil

import           Hogldev.Technique

data DirectionLight = DirectionLight (Vertex3 GLfloat) GLfloat

data LightingTechnique =
    LightingTechnique
    { lightingProgram                          :: !Program
    , lightingWVPLoc                           :: !UniformLocation
    , lightingSamplerLoc                       :: !UniformLocation
    , lightingDirLightColorLoc                 :: !UniformLocation
    , lightingDirLightAmbientIntensityColorLoc :: !UniformLocation
    }

initLightingTechnique :: IO LightingTechnique
initLightingTechnique = do
    program <- createProgram
    addShader program "tutorial17/lighting.vs" VertexShader
    addShader program "tutorial17/lighting.fs" FragmentShader
    finalize program
    wvpLoc <- getUniformLocation program "gWVP"
    samplerLoc <- getUniformLocation program "gSampler"
    dirLightColorLoc <- getUniformLocation program "gDirectionalLight.Color"
    dirLightAmbientIntensityLoc <- getUniformLocation program
        "gDirectionalLight.AmbientIntensity"
    return LightingTechnique
        { lightingProgram                          = program
        , lightingWVPLoc                           = wvpLoc
        , lightingSamplerLoc                       = samplerLoc
        , lightingDirLightColorLoc                 = dirLightColorLoc
        , lightingDirLightAmbientIntensityColorLoc = dirLightAmbientIntensityLoc
        }

setLightingWVP :: LightingTechnique -> [[GLfloat]] -> IO ()
setLightingWVP LightingTechnique{..} mat = uniformMat lightingWVPLoc $= mat

setLightingTextureUnit :: LightingTechnique -> GLuint -> IO ()
setLightingTextureUnit LightingTechnique{..} textureUnit =
    uniformScalar lightingSamplerLoc $= textureUnit

setDirectionalLight :: LightingTechnique -> DirectionLight -> IO ()
setDirectionalLight LightingTechnique{..}
    (DirectionLight (Vertex3 x y z) intensity) = do
    uniformVec lightingDirLightColorLoc $=  [x, y, z]
    uniformScalar lightingDirLightAmbientIntensityColorLoc $= intensity

