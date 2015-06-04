{-# LANGUAGE RecordWildCards #-}
module Hogldev.Pipeline (
    Pipeline(..)
  , initPipeline
  , getWorldTrans
  , getWPTrans
  , PersProj(..)
) where

import           Graphics.Rendering.OpenGL

import           Hogldev.Math3D
import           Hogldev.Utils

data Pipeline = Pipeline
                { scaleInfo  :: Vector3 GLfloat
                , worldPos   :: Vector3 GLfloat
                , rotateInfo :: Vector3 GLfloat
                , persProj   :: PersProj
                }

initPipeline :: Pipeline
initPipeline =
    Pipeline
        (Vector3 1 1 1)
        (Vector3 0 0 0)
        (Vector3 0 0 0)
        (PersProj 0 0 0 0 0)

getWorldTrans :: Pipeline -> Matrix4
getWorldTrans Pipeline{..} =
    translationTrans !*! rotateTrans !*! scaleTrans
  where
    scaleTrans, rotateTrans, translationTrans :: Matrix4
    scaleTrans       = scaleMatrix scaleInfo
    rotateTrans      = initRotateTransform rotateInfo
    translationTrans = translateMatrix worldPos

getWPTrans :: Pipeline -> Matrix4
getWPTrans p@Pipeline{..} = persProjTrans !*! getWorldTrans p
  where
    persProjTrans :: Matrix4
    persProjTrans = perspectiveProj persProj

initRotateTransform :: Vector3 GLfloat -> Matrix4
initRotateTransform (Vector3 x y z) = rz !*! ry !*! rx
  where
    rx, ry, rz :: Matrix4
    rx = rotateXMatrix(toRadian x)
    ry = rotateYMatrix(toRadian y)
    rz = rotateZMatrix(toRadian z)
