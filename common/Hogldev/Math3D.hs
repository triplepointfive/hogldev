{-# LANGUAGE RecordWildCards #-}
module Hogldev.Math3D (
    Matrix4
  , translateMatrix
  , rotateZMatrix
  , rotateYMatrix
  , rotateXMatrix
  , scaleMatrix
  , perspProjTrans
  , cameraRotationTrans
  , (!*!)
) where

import           Graphics.Rendering.OpenGL

import           Data.List

import           Hogldev.Utils

type Matrix4 = [[GLfloat]]

infixl 7 !*!

(!*!) :: Matrix4 -> Matrix4 -> Matrix4
(!*!) a b = [ [ sum $ zipWith (*) ar bc | bc <- transpose b ] | ar <- a ]

translateMatrix :: Vector3 GLfloat -> Matrix4
translateMatrix (Vector3 x y z) =
    [ [ 1, 0, 0, x]
    , [ 0, 1, 0, y]
    , [ 0, 0, 1, z]
    , [ 0, 0, 0, 1]
    ]

rotateZMatrix :: GLfloat -> Matrix4
rotateZMatrix a =
    [ [ cos a, - sin a, 0, 0]
    , [ sin a,   cos a, 0, 0]
    , [     0,       0, 1, 0]
    , [     0,       0, 0, 1]
    ]

rotateYMatrix :: GLfloat -> Matrix4
rotateYMatrix a =
    [ [ cos a, 0, - sin a, 0]
    , [     0, 1,       0, 0]
    , [ sin a, 0,   cos a, 0]
    , [     0, 0,       0, 1]
    ]

rotateXMatrix :: GLfloat -> Matrix4
rotateXMatrix a =
    [ [ 1,     0,       0, 0]
    , [ 0, cos a, - sin a, 0]
    , [ 0, sin a,   cos a, 0]
    , [ 0,     0,       0, 1]
    ]

scaleMatrix :: Vector3 GLfloat -> Matrix4
scaleMatrix (Vector3 x y z) =
    [ [ x, 0, 0, 0]
    , [ 0, y, 0, 0]
    , [ 0, 0, z, 0]
    , [ 0, 0, 0, 1]
    ]

perspProjTrans :: PersProj -> Matrix4
perspProjTrans PersProj{..} =
    [ [ 1 / (tanHalfFOV * ar)
      , 0
      , 0
      , 0]
    , [ 0
      , 1 / tanHalfFOV
      , 0
      , 0]
    , [ 0
      , 0
      , (-persZNear - persZFar) / zRange
      , 2 * persZFar * persZNear / zRange]
    , [ 0
      , 0
      , 1
      , 0]
    ]
  where
    ar = persWidth / persHeigh
    zRange = persZNear - persZFar
    tanHalfFOV = tan (toRadian(persFOV / 2))

cameraRotationTrans :: Camera -> Matrix4
cameraRotationTrans Camera{..} =
    [ [ ux, uy, uz, 0]
    , [ vx, vy, vz, 0]
    , [ nx, ny, nz, 0]
    , [  0,  0,  0, 1]
    ]
  where
    n@(Vector3 nx ny nz) = normalizeVector cameraTarget
    u@(Vector3 ux uy uz) = (normalizeVector cameraUp) `crossVector` n
    (Vector3 vx vy vz) = n `crossVector` u

normalizeVector :: Vector3 GLfloat -> Vector3 GLfloat
normalizeVector (Vector3 x y z) =
    Vector3 (x / vLength) (y / vLength) (z / vLength)
  where
    vLength = sqrt ( x * x + y * y + z * z )

crossVector :: Vector3 GLfloat -> Vector3 GLfloat -> Vector3 GLfloat
crossVector (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 x3 y3 z3
  where
    x3 = y1 * z2 - z1 * y2
    y3 = z1 * x2 - x1 * z2
    z3 = x1 * y2 - y1 * x2
