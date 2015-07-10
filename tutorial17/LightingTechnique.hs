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

vertexShader = unlines
    [ "#version 330"
    , ""
    , "layout (location = 0) in vec3 Position;"
    , "layout (location = 1) in vec2 TexCoord;"
    , ""
    , "uniform mat4 gWVP;"
    , ""
    , "out vec2 TexCoord0;"
    , ""
    , "void main()"
    , "{"
    , "  gl_Position = gWVP * vec4(Position, 1.0);"
    , "  TexCoord0 = TexCoord;"
    , "}"
    ]

fragmentShader = unlines
    [ "#version 330"
    , ""
    , "in vec2 TexCoord0;"
    , ""
    , "struct DirectionalLight"
    , "{"
    , "  vec3 Color;"
    , "  float AmbientIntensity;"
    , "};"
    , ""
    , "uniform DirectionalLight gDirectionalLight;"
    , "uniform sampler2D gSampler;"
    , ""
    , "void main()"
    , "{"
    , "  gl_FragColor = texture2D(gSampler, TexCoord0) *"
    , "               vec4(gDirectionalLight.Color, 1.0f) *"
    , "               gDirectionalLight.AmbientIntensity;"
    , "}"
    ]

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
    addShader program vertexShader VertexShader
    addShader program fragmentShader FragmentShader
    finalize program
    wvpLoc <- getUniformLocation program "gWVP"
    samplerLoc <- getUniformLocation program "gSampler"
    dirLightColorLoc <- getUniformLocation program "gDirectionalLight.Color"
    dirLightAmbientIntensityLoc <- getUniformLocation program
        "gDirectionalLight.AmbientIntensity"
    return $ LightingTechnique
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
