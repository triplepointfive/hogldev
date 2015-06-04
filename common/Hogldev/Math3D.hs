{-# LANGUAGE RecordWildCards #-}
module Hogldev.Math3D (
    Matrix4
  , translateMatrix
  , rotateZMatrix
  , rotateYMatrix
  , rotateXMatrix
  , scaleMatrix
  , perspectiveProj
  , (!*!)
) where

import           Graphics.Rendering.OpenGL

import           Data.List

import           Hogldev.Utils

type Matrix4 = [[GLfloat]]

infixl 7 !*!

(!*!) :: Matrix4 -> Matrix4 -> Matrix4
(!*!) a b = [ [ sum $ zipWith (*) ar bc | bc <- (transpose b) ] | ar <- a ]

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

perspectiveProj :: PersProj -> Matrix4
perspectiveProj PersProj{..} =
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
