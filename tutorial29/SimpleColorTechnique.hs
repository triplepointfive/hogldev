{-# LANGUAGE RecordWildCards #-}
module SimpleColorTechnique (
    SimpleColorTechnique (..)
  , initSimpleColorTechnique
  , setSimpleColorTechniqueWVP
  , enableSimpleColorTechnique
) where

import           Graphics.GLUtil
import           Graphics.Rendering.OpenGL

import           Hogldev.Technique

data SimpleColorTechnique =
    SimpleColorTechnique
    { lProgram     :: !Program
    , lWVPLocation :: !UniformLocation
    } deriving Show

initSimpleColorTechnique :: IO SimpleColorTechnique
initSimpleColorTechnique = do
    program <- createProgram
    addShader program "tutorial29/simple_color.vs" VertexShader
    addShader program "tutorial29/simple_color.fs" FragmentShader
    finalize program

    wvpLoc <- getUniformLocation program "gWVP"

    return SimpleColorTechnique
        { lProgram     = program
        , lWVPLocation = wvpLoc
        }

setSimpleColorTechniqueWVP :: SimpleColorTechnique -> [[GLfloat]] -> IO ()
setSimpleColorTechniqueWVP SimpleColorTechnique{..} mat
    = uniformMat lWVPLocation $= mat

enableSimpleColorTechnique :: SimpleColorTechnique -> IO ()
enableSimpleColorTechnique SimpleColorTechnique{..} = enableTechnique lProgram
