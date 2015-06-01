module Hogldev.Math3D (
    Matrix4
  , translateMatrix
  , rotateZMatrix
) where

import           Graphics.Rendering.OpenGL

type Matrix4 = GLmatrix GLfloat

translateMatrix :: GLfloat
                -> GLfloat
                -> GLfloat
                -> IO Matrix4
translateMatrix x y z =
    newMatrix ColumnMajor
        [ 1, 0, 0, x
        , 0, 1, 0, y
        , 0, 0, 1, z
        , 0, 0, 0, 1
        ]

rotateZMatrix :: GLfloat
              -> IO Matrix4
rotateZMatrix a =
    newMatrix ColumnMajor
        [ cos a, - sin a, 0, 0
        , sin a,   cos a, 0, 0
        ,     0,       0, 1, 0
        ,     0,       0, 0, 1
        ]
