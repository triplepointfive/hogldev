module Hogldev.Utils (
    toRadian
  , toDegree
) where

toRadian :: (Floating a, Num a) => a -> a
toRadian x = x * pi / 180

toDegree :: (Floating a, Num a) => a -> a
toDegree x = x * 180 / pi
