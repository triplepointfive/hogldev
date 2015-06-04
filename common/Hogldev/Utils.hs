module Hogldev.Utils (
    toRadian
  , toDegree
  , bufferOffset
  , PersProj(..)
) where

import           Graphics.Rendering.OpenGL
import           Foreign.Ptr

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

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
