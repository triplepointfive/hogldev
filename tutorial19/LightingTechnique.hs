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


import           Graphics.GLUtil
import           Graphics.Rendering.OpenGL

import           Hogldev.Technique

vertexShader = unlines
    [ "#version 330"
    , ""
    , "layout (location = 0) in vec3 Position;"
    , "layout (location = 1) in vec2 TexCoord;"
    , "layout (location = 2) in vec3 Normal;"
    , ""
    , "uniform mat4 gWVP;"
    , "uniform mat4 gWorld;"
    , ""
    , "out vec2 TexCoord0;"
    , "out vec3 Normal0;"
    , ""
    , "void main()"
    , "{"
    , "  gl_Position = gWVP * vec4(Position, 1.0);"
    , "  TexCoord0 = TexCoord;"
    , "  Normal0 = (gWorld * vec4(Normal, 0.0)).xyz;"
    , "}"
    ]

fragmentShader = unlines
    [ "#version 330"
    , ""
    , "in vec2 TexCoord0;"
    , "in vec3 Normal0;"
    , ""
    , "struct DirectionalLight"
    , "{"
    , "  vec3 Color;"
    , "  float AmbientIntensity;"
    , "  float DiffuseIntensity;"
    , "  vec3 Direction;"
    , "};"
    , ""
    , "uniform DirectionalLight gDirectionalLight;"
    , "uniform sampler2D gSampler;"
    , ""
    , "void main()"
    , "{"
    , "  vec4 AmbientColor = vec4(gDirectionalLight.Color, 1.0f) *"
    , "    gDirectionalLight.AmbientIntensity;"
    , ""
    , "  float DiffuseFactor = dot(normalize(Normal0),"
    , "    -gDirectionalLight.Direction);"
    , ""
    , "  vec4 DiffuseColor;"
    , ""
    , "  if (DiffuseFactor > 0) {"
    , "    DiffuseColor = vec4(gDirectionalLight.Color, 1.0f) *"
    , "      gDirectionalLight.DiffuseIntensity *"
    , "      DiffuseFactor;"
    , "  } else {"
    , "    DiffuseColor = vec4(0,0,0,0);"
    , "  }"
    , ""
    , "  gl_FragColor = texture2D(gSampler, TexCoord0) *"
    , "    (AmbientColor + DiffuseColor);"
    , "}"
    ]

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
    addShader program vertexShader VertexShader
    addShader program fragmentShader FragmentShader
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

