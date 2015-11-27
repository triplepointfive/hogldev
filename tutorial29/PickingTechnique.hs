{-# LANGUAGE RecordWildCards #-}
module PickingTechnique (
    PickingTechnique (..)
  , initPickingTechnique
  , setPickingTechniqueWVP
  , setPickingObjectIndex
  , setPickingTechniqueDrawStartCB
  , enablePickingTechnique
) where

import           Graphics.GLUtil
import           Graphics.Rendering.OpenGL

import           Hogldev.Technique

data PickingTechnique =
    PickingTechnique
    { lProgram        :: !Program
    , lWVPLoc         :: !UniformLocation
    , lDrawIndexLoc   :: !UniformLocation
    , lObjectIndexLoc :: !UniformLocation
    } deriving Show

initPickingTechnique :: IO PickingTechnique
initPickingTechnique = do
    program <- createProgram
    addShader program "tutorial29/picking.vs" VertexShader
    addShader program "tutorial29/picking.fs" FragmentShader
    finalize program

    wvpLoc <- getUniformLocation program "gWVP"
    drawIndexLoc <- getUniformLocation program "gObjectIndex"
    objectIndexLoc <- getUniformLocation program "gDrawIndex"

    return PickingTechnique
        { lProgram        = program
        , lWVPLoc         = wvpLoc
        , lDrawIndexLoc   = drawIndexLoc
        , lObjectIndexLoc = objectIndexLoc
        }

setPickingTechniqueWVP :: PickingTechnique -> [[GLfloat]] -> IO ()
setPickingTechniqueWVP PickingTechnique{..} mat =
    uniformMat lWVPLoc $= mat

setPickingObjectIndex :: PickingTechnique -> GLuint -> IO ()
setPickingObjectIndex PickingTechnique{..} objectIndex =
    uniformScalar lObjectIndexLoc $= objectIndex

setPickingTechniqueDrawStartCB :: PickingTechnique -> GLuint -> IO ()
setPickingTechniqueDrawStartCB PickingTechnique{..} drawIndex =
    uniformScalar lDrawIndexLoc $= drawIndex

enablePickingTechnique :: PickingTechnique -> IO ()
enablePickingTechnique PickingTechnique{..} = enableTechnique lProgram
