module Hogldev.Utils (
    toRadian
  , toDegree
  , bufferOffset
  , PersProj(..)
  , normalizeVector
  , normalizeVertex
  , rotateVector
  , wrapMaybe
) where

import           Graphics.Rendering.OpenGL hiding (rotate)
import           Foreign.Ptr
import           Linear (Quaternion(..), V3(..), rotate)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

instance (Num a) => Num (Vertex3 a) where
  (+) (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) =
      Vertex3 (x1 + x2) (y1 + y2) (z1 + z2)
  (-) (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) =
      Vertex3 (x1 - x2) (y1 - y2) (z1 - z2)
  (*) (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = Vertex3
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)
  abs = fmap abs
  signum = error "signum called for Vertex3"
  fromInteger = error "fromInteger called for Vertex3"

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

instance (Num a) => Num (Vector2 a) where
  (+) (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (x1 + x2) (y1 + y2)
  (-) (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (x1 - x2) (y1 - y2)
  abs = fmap abs
  (*) _ _ = error "(*) called for Vector2"
  signum = error "signum called for Vector2"
  fromInteger = error "fromInteger called for Vector2"

normalizeVertex :: Vertex3 GLfloat -> Vertex3 GLfloat
normalizeVertex (Vertex3 x y z) =
    Vertex3 (x / vLength) (y / vLength) (z / vLength)
  where
    vLength = sqrt ( x * x + y * y + z * z )

normalizeVector :: Vector3 GLfloat -> Vector3 GLfloat
normalizeVector (Vector3 x y z) =
    Vector3 (x / vLength) (y / vLength) (z / vLength)
  where
    vLength = sqrt ( x * x + y * y + z * z )

rotateVector :: GLfloat -> Vector3 GLfloat -> Vector3 GLfloat -> Vector3 GLfloat
rotateVector angle (Vector3 originX originY originZ) (Vector3 axeX axeY axeZ) =
    Vector3 x y z
  where
    sinHalfAngle = sin (toRadian (angle / 2))
    cosHalfAngle = cos (toRadian (angle / 2))

    qvec = V3 (axeX * sinHalfAngle) (axeY * sinHalfAngle) (axeZ * sinHalfAngle)
    rotationQ = Quaternion cosHalfAngle qvec

    V3 x y z = rotate rotationQ (V3 originX originY originZ)

data PersProj = PersProj
    { persFOV   :: !GLfloat
    , persWidth :: !GLfloat
    , persHeigh :: !GLfloat
    , persZNear :: !GLfloat
    , persZFar  :: !GLfloat
    } deriving Show

toRadian :: (Floating a, Num a) => a -> a
toRadian x = x * pi / 180

toDegree :: (Floating a, Num a) => a -> a
toDegree x = x * 180 / pi

wrapMaybe :: Bool -> a -> Maybe a
wrapMaybe True  v = Just v
wrapMaybe False _ = Nothing
