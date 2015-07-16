{-# LANGUAGE RecordWildCards #-}
module LightingTechnique (
    LightingTechnique(..)
  , DirectionLight(..)
  , changeAmbIntensity
  , changeDiffIntensity
  , initLightingTechnique
  , setLightingWVP
  , setLightingWorldMatrix
  , setLightingTextureUnit
  , setDirectionalLight
) where


import           Graphics.Rendering.OpenGL
import           Graphics.GLUtil

import           Hogldev.Technique

data DirectionLight =
    DirectionLight
    { ambientColor     :: !(Vertex3 GLfloat)
    , ambientIntensity :: !GLfloat
    , diffuseDirection :: !(Vertex3 GLfloat)
    , diffuseIntensity :: !GLfloat
    } deriving Show

changeAmbIntensity :: (GLfloat -> GLfloat) -> DirectionLight -> DirectionLight
changeAmbIntensity f dir@DirectionLight{..} =
    dir { ambientIntensity = f ambientIntensity }

changeDiffIntensity :: (GLfloat -> GLfloat) -> DirectionLight -> DirectionLight
changeDiffIntensity f dir@DirectionLight{..} =
    dir { diffuseIntensity = f diffuseIntensity }

data LightingTechnique =
    LightingTechnique
    { lProgram                          :: !Program
    , lWVPLoc                           :: !UniformLocation
    , lWorldMatrixLoc                   :: !UniformLocation
    , lSamplerLoc                       :: !UniformLocation
    , lDirLightColorLoc                 :: !UniformLocation
    , lDirLightAmbientIntensityColorLoc :: !UniformLocation
    , lDirLightDirectionLoc             :: !UniformLocation
    , lDirLightIntensity                :: !UniformLocation
    }

initLightingTechnique :: IO LightingTechnique
initLightingTechnique = do
    program <- createProgram
    addShader program "tutorial18/lighting.vs" VertexShader
    addShader program "tutorial18/lighting.fs" FragmentShader
    finalize program

    wvpLoc <- getUniformLocation program "gWVP"
    worldMatrixLoc <- getUniformLocation program "gWorld"
    samplerLoc <- getUniformLocation program "gSampler"
    dirLightColorLoc <- getUniformLocation program "gDirectionalLight.Color"
    dirLightAmbientIntensityLoc <- getUniformLocation program
        "gDirectionalLight.AmbientIntensity"
    dirLightDirectionLoc <- getUniformLocation program
        "gDirectionalLight.Direction"
    dirLightDiffuseIntensity <- getUniformLocation program
        "gDirectionalLight.DiffuseIntensity"

    return LightingTechnique
        { lProgram                          = program
        , lWVPLoc                           = wvpLoc
        , lWorldMatrixLoc                   = worldMatrixLoc
        , lSamplerLoc                       = samplerLoc
        , lDirLightColorLoc                 = dirLightColorLoc
        , lDirLightAmbientIntensityColorLoc = dirLightAmbientIntensityLoc
        , lDirLightDirectionLoc             = dirLightDirectionLoc
        , lDirLightIntensity                = dirLightDiffuseIntensity
        }

setLightingWVP :: LightingTechnique -> [[GLfloat]] -> IO ()
setLightingWVP LightingTechnique{..} mat = uniformMat lWVPLoc $= mat

setLightingWorldMatrix :: LightingTechnique -> [[GLfloat]] -> IO ()
setLightingWorldMatrix LightingTechnique{..} mat =
    uniformMat lWorldMatrixLoc $= mat

setLightingTextureUnit :: LightingTechnique -> GLuint -> IO ()
setLightingTextureUnit LightingTechnique{..} textureUnit =
    uniformScalar lSamplerLoc $= textureUnit

setDirectionalLight :: LightingTechnique -> DirectionLight -> IO ()
setDirectionalLight LightingTechnique{..} DirectionLight{..} = do
    uniformVec lDirLightColorLoc $= [cx, cy, cz]
    uniformScalar lDirLightAmbientIntensityColorLoc $= ambientIntensity
    uniformVec lDirLightDirectionLoc $= [lx, ly, lz]
    uniformScalar lDirLightIntensity $= diffuseIntensity
  where
    (Vertex3 cx cy cz) = ambientColor
    (Vertex3 lx ly lz) = diffuseDirection

