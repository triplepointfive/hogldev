{-# LANGUAGE RecordWildCards #-}
module BillboardTechnique (
    BillboardTechnique (..)
  , initBillboardTechnique
  , setBillboardTechniqueVP
  , setBillboardTechniqueColorUnit
  , setBillboardCameraPosition
  , enableBillboardTechnique
) where

import           Graphics.GLUtil
import           Graphics.Rendering.OpenGL

import           Hogldev.Technique

data BillboardTechnique =
    BillboardTechnique
    { lProgram     :: !Program
    , lVPLocation  :: !UniformLocation
    , lCameraPos   :: !UniformLocation
    , lColorMapLoc :: !UniformLocation
    } deriving Show

initBillboardTechnique :: IO BillboardTechnique
initBillboardTechnique = do
    program <- createProgram
    addShader program "tutorial28/billboard.vs" VertexShader
    addShader program "tutorial28/billboard.fs" FragmentShader
    addShader program "tutorial28/billboard.gs" GeometryShader
    finalize program

    wvpLoc <- getUniformLocation program "gVP"
    cameraPosLoc <- getUniformLocation program "gCameraPos"
    colorMapLoc <- getUniformLocation program "gColorMap"

    return BillboardTechnique
        { lProgram     = program
        , lVPLocation  = wvpLoc
        , lCameraPos   = cameraPosLoc
        , lColorMapLoc = colorMapLoc
        }

setBillboardTechniqueVP :: BillboardTechnique -> [[GLfloat]] -> IO ()
setBillboardTechniqueVP BillboardTechnique{..} mat
    = uniformMat lVPLocation $= mat

setBillboardCameraPosition :: BillboardTechnique -> Vector3 GLfloat -> IO ()
setBillboardCameraPosition BillboardTechnique{..} (Vector3 x y z)
    = uniformVec lCameraPos $= [x, y, z]

setBillboardTechniqueColorUnit :: BillboardTechnique -> GLint -> IO ()
setBillboardTechniqueColorUnit BillboardTechnique{..} textureUnit =
    uniformScalar lColorMapLoc $= textureUnit

enableBillboardTechnique :: BillboardTechnique -> IO ()
enableBillboardTechnique BillboardTechnique{..} = enableTechnique lProgram
