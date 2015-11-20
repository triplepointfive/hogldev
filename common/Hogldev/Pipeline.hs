{-# LANGUAGE RecordWildCards #-}
module Hogldev.Pipeline (
    Pipeline(..)
  , getTrans
  , PersProj(..)
  , Camera(..)
) where

import           Graphics.Rendering.OpenGL

import           Hogldev.Math3D
import           Hogldev.Utils
import           Hogldev.Camera

data Pipeline
    = WPipeline
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
    | WVPPipeline
        { scaleInfo  :: Vector3 GLfloat
        , worldInfo  :: Vector3 GLfloat
        , rotateInfo :: Vector3 GLfloat
        , persProj   :: PersProj
        , pipeCamera :: Camera
        }
    | VPPipeline
        { persProj   :: PersProj
        , pipeCamera :: Camera
        }
    deriving Show

getTrans :: Pipeline -> Matrix4
getTrans WPipeline{..}   = worldTrans scaleInfo worldInfo rotateInfo
getTrans WPPipeline{..}  = projTrans scaleInfo worldInfo rotateInfo persProj
getTrans VPPipeline{..} = vpTrans persProj pipeCamera
getTrans WVPPipeline{..} =
    projViewTrans scaleInfo worldInfo rotateInfo persProj pipeCamera

vpTrans :: PersProj -> Camera -> Matrix4
vpTrans persProj camera = perspProjTrans persProj !*! cameraTrans camera

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

projTrans :: Vector3 GLfloat
          -> Vector3 GLfloat
          -> Vector3 GLfloat
          -> PersProj
          -> Matrix4
projTrans scaleInfo worldInfo rotateInfo persProj =
    perspProjTrans persProj !*! worldTrans scaleInfo worldInfo rotateInfo

projViewTrans :: Vector3 GLfloat
              -> Vector3 GLfloat
              -> Vector3 GLfloat
              -> PersProj
              -> Camera
              -> Matrix4
projViewTrans scaleInfo worldInfo rotateInfo persProj camera =
    perspProjTrans persProj
        !*! cameraTrans camera
        !*! worldTrans scaleInfo worldInfo rotateInfo

cameraTrans :: Camera -> Matrix4
cameraTrans c@Camera{..} =
    cameraRotationTrans c !*! translateMatrix (fmap (*(-1) ) cameraPos)

initRotateTransform :: Vector3 GLfloat -> Matrix4
initRotateTransform (Vector3 x y z) = rz !*! ry !*! rx
  where
    rx, ry, rz :: Matrix4
    rx = rotateXMatrix(toRadian x)
    ry = rotateYMatrix(toRadian y)
    rz = rotateZMatrix(toRadian z)
