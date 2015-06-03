{-# LANGUAGE RecordWildCards #-}
module Hogldev.Pipeline (
    Pipeline(..)
  , initPipeline
  , getTrans
) where

import           Graphics.Rendering.OpenGL

import           Hogldev.Math3D
import           Hogldev.Utils

data Pipeline = Pipeline
                { scale      :: Vector3 GLfloat
                , worldPos   :: Vector3 GLfloat
                , rotateInfo :: Vector3 GLfloat
                }

initPipeline :: Pipeline
initPipeline = Pipeline (Vector3 1 1 1) (Vector3 0 0 0) (Vector3 0 0 0)

getTrans :: Pipeline -> Matrix4
getTrans Pipeline{..} = do
    translationTrans !*! rotateTrans !*! scaleTrans
  where
    scaleTrans, rotateTrans, translationTrans :: Matrix4
    scaleTrans       = scaleMatrix scale
    rotateTrans      = initRotateTransform rotateInfo
    translationTrans = translateMatrix worldPos

initRotateTransform :: Vector3 GLfloat -> Matrix4
initRotateTransform (Vector3 x y z) = rz !*! ry !*! rx
  where
    rx, ry, rz :: Matrix4
    rx = rotateXMatrix(toRadian x)
    ry = rotateYMatrix(toRadian y)
    rz = rotateZMatrix(toRadian z)
