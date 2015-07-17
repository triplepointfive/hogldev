{-# LANGUAGE RecordWildCards #-}
module LightingTechnique (
    LightingTechnique(..)
  , DirectionLight(..)
  , PointLight(..)
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
) where

import           Graphics.GLUtil
import           Graphics.Rendering.OpenGL

import           Hogldev.Technique

maxPointLights = 3

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

initLightingTechnique :: IO LightingTechnique
initLightingTechnique = do
    program <- createProgram
    addShader program "tutorial20/lighting.vs" VertexShader
    addShader program "tutorial20/lighting.fs" FragmentShader
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

