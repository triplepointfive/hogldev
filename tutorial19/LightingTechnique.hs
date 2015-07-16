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
  , setEyeWorldPos
  , setMatSpecularPower
  , setMaterialSpecularIntensity
) where


import           Graphics.GLUtil
import           Graphics.Rendering.OpenGL

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
    , lEyeWorldPosLoc                   :: !UniformLocation
    , lMatSpecularIntensityLoc          :: !UniformLocation
    , lMatSpecularPowerLoc              :: !UniformLocation
    }

initLightingTechnique :: IO LightingTechnique
initLightingTechnique = do
    program <- createProgram
    addShader program "tutorial19/lighting.vs" VertexShader
    addShader program "tutorial19/lighting.fs" FragmentShader
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

    eyeWorldPosition <- getUniformLocation program "gEyeWorldPos"
    matSpecularIntensity <- getUniformLocation program "gMatSpecularIntensity"
    matSpecularPower <- getUniformLocation program "gSpecularPower"

    return LightingTechnique
        { lProgram                          = program
        , lWVPLoc                           = wvpLoc
        , lWorldMatrixLoc                   = worldMatrixLoc
        , lSamplerLoc                       = samplerLoc
        , lDirLightColorLoc                 = dirLightColorLoc
        , lDirLightAmbientIntensityColorLoc = dirLightAmbientIntensityLoc
        , lDirLightDirectionLoc             = dirLightDirectionLoc
        , lDirLightIntensity                = dirLightDiffuseIntensity
        , lEyeWorldPosLoc                   = matSpecularIntensity
        , lMatSpecularIntensityLoc          = eyeWorldPosition
        , lMatSpecularPowerLoc              = matSpecularPower
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

setMaterialSpecularIntensity :: LightingTechnique -> GLfloat -> IO ()
setMaterialSpecularIntensity LightingTechnique{..} intensity =
    uniformScalar lSamplerLoc $= intensity

setMatSpecularPower :: LightingTechnique -> GLfloat -> IO ()
setMatSpecularPower LightingTechnique{..} power =
    uniformScalar lMatSpecularPowerLoc $= power

setEyeWorldPos :: LightingTechnique -> Vector3 GLfloat -> IO ()
setEyeWorldPos LightingTechnique{..} (Vector3 x y z) =
    uniformVec lEyeWorldPosLoc $= [x, y, z]

