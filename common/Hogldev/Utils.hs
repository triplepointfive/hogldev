module Hogldev.Utils (
    toRadian
  , toDegree
  , bufferOffset
  , PersProj(..)
  , normalizeVector
) where

import           Graphics.Rendering.OpenGL
import           Foreign.Ptr

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

instance (Num a) => Num (Vector3 a) where
  (+) (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
      Vector3 (x1 + x2) (y1 + y2) (z1 + z2)
  (-) (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
      Vector3 (x1 - x2) (y1 - y2) (z1 - z2)
  (*) (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)
  abs = fmap abs
  signum = error "signum called for Vector3"
  fromInteger = error "fromInteger called for Vector3"

normalizeVector :: Vector3 GLfloat -> Vector3 GLfloat
normalizeVector (Vector3 x y z) =
    Vector3 (x / vLength) (y / vLength) (z / vLength)
  where
    vLength = sqrt ( x * x + y * y + z * z )

data PersProj = PersProj
                { persFOV   :: !GLfloat
                , persWidth :: !GLfloat
                , persHeigh :: !GLfloat
                , persZNear :: !GLfloat
                , persZFar  :: !GLfloat
                }

toRadian :: (Floating a, Num a) => a -> a
toRadian x = x * pi / 180

toDegree :: (Floating a, Num a) => a -> a
toDegree x = x * 180 / pi
