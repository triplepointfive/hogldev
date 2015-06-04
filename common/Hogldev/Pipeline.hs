{-# LANGUAGE RecordWildCards #-}
module Hogldev.Pipeline (
    Pipeline(..)
  , getTrans
  , PersProj(..)
) where

import           Graphics.Rendering.OpenGL

import           Hogldev.Math3D
import           Hogldev.Utils

data Pipeline =
    WPipeline
        { scaleInfo  :: Vector3 GLfloat
        , worldInfo  :: Vector3 GLfloat
        , rotateInfo :: Vector3 GLfloat
        }
    | WPPipeline
        { scaleInfo  :: Vector3 GLfloat
        , worldInfo  :: Vector3 GLfloat
        , rotateInfo :: Vector3 GLfloat
        , persProj   :: PersProj
        }
--    WVPPipeline

getTrans :: Pipeline -> Matrix4
getTrans WPipeline{..} = worldTrans scaleInfo worldInfo rotateInfo
getTrans WPPipeline{..} =
    perspectiveProj persProj !*! worldTrans scaleInfo worldInfo rotateInfo

worldTrans :: Vector3 GLfloat
           -> Vector3 GLfloat
           -> Vector3 GLfloat
           -> Matrix4
worldTrans scaleInfo worldInfo rotateInfo =
    translationTrans !*! rotateTrans !*! scaleTrans
  where
    scaleTrans, rotateTrans, translationTrans :: Matrix4
    scaleTrans       = scaleMatrix scaleInfo
    rotateTrans      = initRotateTransform rotateInfo
    translationTrans = translateMatrix worldInfo

initRotateTransform :: Vector3 GLfloat -> Matrix4
initRotateTransform (Vector3 x y z) = rz !*! ry !*! rx
  where
    rx, ry, rz :: Matrix4
    rx = rotateXMatrix(toRadian x)
    ry = rotateYMatrix(toRadian y)
    rz = rotateZMatrix(toRadian z)
