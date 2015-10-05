{-# LANGUAGE RecordWildCards #-}
module Hogldev.LightingTechnique (
    LightingTechnique(..)
  , DirectionLight(..)
  , PointLight(..)
  , DirectionalLight(..)
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
  , setPointLights
  , setSpotLights
  , enableLightingTechnique
) where

import           Graphics.GLUtil
import           Graphics.Rendering.OpenGL

import           Hogldev.Technique
import           Hogldev.Utils (normalizeVertex, toRadian)

maxPointLights = 2
maxSpotLights  = 2

data DirectionLight =
    DirectionLight
    { ambientColor     :: !(Vertex3 GLfloat)
    , ambientIntensity :: !GLfloat
    , diffuseDirection :: !(Vertex3 GLfloat)
    , diffuseIntensity :: !GLfloat
    } deriving Show

data PointLight =
    PointLight
    { pAmbientColor     :: !(Vertex3 GLfloat)
    , pAmbientIntensity :: !GLfloat
    , pDiffuseIntensity :: !GLfloat
    , pPosition         :: !(Vertex3 GLfloat)
    , pConstant         :: !GLfloat
    , pLinear           :: !GLfloat
    , pExp              :: !GLfloat
    } deriving Show

data DirectionalLight =
    DirectionalLight
    { dAmbientColor     :: !(Vertex3 GLfloat)
    , dAmbientIntensity :: !GLfloat
    , dDiffuseIntensity :: !GLfloat
    , dPosition         :: !(Vertex3 GLfloat)
    , dConstant         :: !GLfloat
    , dLinear           :: !GLfloat
    , dExp              :: !GLfloat
    , dDirection        :: !(Vertex3 GLfloat)
    , dCutOff           :: !GLfloat
    }

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
    , lNumPointLightsLoc                :: !UniformLocation
    , lPointLightLocs                   :: ![PointLightLoc]
    , lNumSpotLightsLoc                 :: !UniformLocation
    , lSpotLightLocs                    :: ![DirectionalLightLoc]
    }

data PointLightLoc =
    PointLightLoc
    { plColorLoc            :: !UniformLocation
    , plAmbientIntensityLoc :: !UniformLocation
    , plDiffuseIntensityLoc :: !UniformLocation
    , plPositionLoc         :: !UniformLocation
    , plConstantLoc         :: !UniformLocation
    , plLinearLoc           :: !UniformLocation
    , plExpLoc              :: !UniformLocation
    }

data DirectionalLightLoc =
    DirectionalLightLoc
    { dlColorLoc            :: !UniformLocation
    , dlAmbientIntensityLoc :: !UniformLocation
    , dlDiffuseIntensityLoc :: !UniformLocation
    , dlPositionLoc         :: !UniformLocation
    , dlConstantLoc         :: !UniformLocation
    , dlLinearLoc           :: !UniformLocation
    , dlExpLoc              :: !UniformLocation
    , dlDirectionLoc        :: !UniformLocation
    , dlCutOffLoc           :: !UniformLocation
    }

initLightingTechnique :: IO LightingTechnique
initLightingTechnique = do
    program <- createProgram
    addShader program "tutorial21/lighting.vs" VertexShader
    addShader program "tutorial21/lighting.fs" FragmentShader
    finalize program

    wvpLoc <- getUniformLocation program "gWVP"
    worldMatrixLoc <- getUniformLocation program "gWorld"
    samplerLoc <- getUniformLocation program "gSampler"
    eyeWorldPosition <- getUniformLocation program "gEyeWorldPos"

    dirLightColorLoc <- getUniformLocation program "gDirectionalLight.Base.Color"
    dirLightAmbientIntensityLoc <- getUniformLocation program
        "gDirectionalLight.Base.AmbientIntensity"
    dirLightDirectionLoc <- getUniformLocation program
        "gDirectionalLight.Direction"
    dirLightDiffuseIntensity <- getUniformLocation program
        "gDirectionalLight.Base.DiffuseIntensity"

    matSpecularIntensity <- getUniformLocation program "gMatSpecularIntensity"
    matSpecularPower <- getUniformLocation program "gSpecularPower"

    numPointLightsLoc <- getUniformLocation program "gNumPointLights"
    pointLights <- mapM (pointLightLoc program) [0..maxPointLights-1]

    numSpotLights <- getUniformLocation program "gNumSpotLights"
    spotLights <- mapM (spotLightLoc program) [0..maxSpotLights-1]

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
        , lNumPointLightsLoc                = numPointLightsLoc
        , lPointLightLocs                   = pointLights
        , lNumSpotLightsLoc                 = numSpotLights
        , lSpotLightLocs                    = spotLights
        }

pointLightLoc :: Program -> Int -> IO PointLightLoc
pointLightLoc program index = do
    lColor            <- loc "Base.Color"
    lAmbientIntensity <- loc "Base.AmbientIntensity"
    lPosition         <- loc "Position"
    lDiffuseIntensity <- loc "Base.DiffuseIntensity"
    lConstant         <- loc "Atten.Constant"
    lLinear           <- loc "Atten.Linear"
    lExp              <- loc "Atten.Exp"
    return PointLightLoc
        { plColorLoc            = lColor
        , plAmbientIntensityLoc = lAmbientIntensity
        , plDiffuseIntensityLoc = lDiffuseIntensity
        , plPositionLoc         = lPosition
        , plConstantLoc         = lConstant
        , plLinearLoc           = lLinear
        , plExpLoc              = lExp
        }
  where
    loc field = getUniformLocation program
        ("gPointLights[" ++ show index ++ "]." ++ field)

spotLightLoc :: Program -> Int -> IO DirectionalLightLoc
spotLightLoc program index = do
    dlColorLoc            <- loc "Base.Base.Color"
    dlAmbientIntensityLoc <- loc "Base.Base.AmbientIntensity"
    dlDiffuseIntensityLoc <- loc "Base.Base.DiffuseIntensity"
    dlPositionLoc         <- loc "Base.Position"
    dlConstantLoc         <- loc "Base.Atten.Constant"
    dlLinearLoc           <- loc "Base.Atten.Linear"
    dlExpLoc              <- loc "Base.Atten.Exp"
    dlDirectionLoc        <- loc "Direction"
    dlCutOffLoc           <- loc "Cutoff"
    return DirectionalLightLoc
        { dlColorLoc            = dlColorLoc
        , dlAmbientIntensityLoc = dlAmbientIntensityLoc
        , dlDiffuseIntensityLoc = dlDiffuseIntensityLoc
        , dlPositionLoc         = dlPositionLoc
        , dlConstantLoc         = dlConstantLoc
        , dlLinearLoc           = dlLinearLoc
        , dlExpLoc              = dlExpLoc
        , dlDirectionLoc        = dlDirectionLoc
        , dlCutOffLoc           = dlCutOffLoc
        }
  where
    loc field = getUniformLocation program
        ("gSpotLights[" ++ show index ++ "]." ++ field)

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

setPointLights :: LightingTechnique -> GLint -> [PointLight] -> IO ()
setPointLights LightingTechnique{..} numLights pointLights = do
    uniformScalar lNumPointLightsLoc $= numLights
    mapM_ (uncurry setPointLight) (zip pointLights lPointLightLocs)
  where
    setPointLight :: PointLight -> PointLightLoc -> IO ()
    setPointLight PointLight{..} PointLightLoc{..} = do
        uniformVertex3 plColorLoc pAmbientColor
        uniformVertex3 plPositionLoc pPosition
        uniformScalar plAmbientIntensityLoc $= pAmbientIntensity
        uniformScalar plDiffuseIntensityLoc $= pDiffuseIntensity
        uniformScalar plConstantLoc         $= pConstant
        uniformScalar plLinearLoc           $= pLinear
        uniformScalar plExpLoc              $= pExp

uniformVertex3 :: UniformLocation -> Vertex3 GLfloat -> IO ()
uniformVertex3 loc (Vertex3 x y z) = uniformVec loc $= [x, y, z]

setSpotLights :: LightingTechnique -> GLint -> [DirectionalLight] ->  IO ()
setSpotLights LightingTechnique{..} numLights spotLights = do
    uniformScalar lNumSpotLightsLoc $= numLights
    mapM_ (uncurry setSpotLight) (zip spotLights lSpotLightLocs)
  where
    setSpotLight :: DirectionalLight -> DirectionalLightLoc -> IO ()
    setSpotLight DirectionalLight{..}  DirectionalLightLoc{..} = do
        uniformVertex3 dlColorLoc dAmbientColor
        uniformVertex3 dlPositionLoc dPosition
        uniformVertex3 dlDirectionLoc (normalizeVertex dDirection)
        uniformScalar dlAmbientIntensityLoc $= dAmbientIntensity
        uniformScalar dlDiffuseIntensityLoc $= dDiffuseIntensity
        uniformScalar dlConstantLoc         $= dConstant
        uniformScalar dlLinearLoc           $= dLinear
        uniformScalar dlExpLoc              $= dExp
        uniformScalar dlCutOffLoc           $= cos (toRadian dCutOff)

enableLightingTechnique :: LightingTechnique -> IO ()
enableLightingTechnique LightingTechnique{..} = enableTechnique lProgram
