{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Hogldev.Camera (
    Camera(..)
  , cameraOnKeyboard
  , cameraOnMouse
  , cameraOnRender
  , initCamera
) where

import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLUT (SpecialKey(..))

import           Hogldev.Utils

import           Data.Maybe (catMaybes)

stepScale = 0.1
rotationScale = 0.05
edgeStep = 0.1
margin = 10

data Edge = UpperEdge
          | LowerEdge
          | LeftEdge
          | RightEdge
          deriving (Show, Eq, Enum)

data Camera = Camera
              { cameraPos          :: !(Vector3 GLfloat)
              , cameraTarget       :: !(Vector3 GLfloat)
              , cameraUp           :: !(Vector3 GLfloat)
              , cameraWindowWidth  :: !GLsizei
              , cameraWindowHeight :: !GLsizei
              , cameraAngleH       :: !GLfloat
              , cameraAngleV       :: !GLfloat
              , cameraMousePos     :: !(Vector2 GLint)
              , cameraOnEdges      :: ![Edge]
              }
              deriving Show

initCamera :: Maybe (Vector3 GLfloat, Vector3 GLfloat, Vector3 GLfloat)
           -> GLsizei
           -> GLsizei
           -> Camera
initCamera Nothing width height = initWithLocation
    (Vector3 0 0 0) (Vector3 0 0 1) (Vector3 0 1 0) width height
initCamera (Just (pos, target, up)) width height =
    initWithLocation pos target up width height

initWithLocation :: Vector3 GLfloat
                 -> Vector3 GLfloat
                 -> Vector3 GLfloat
                 -> GLsizei
                 -> GLsizei
                 -> Camera
initWithLocation pos target up width height =
    Camera pos target up width height angleH angleV mousePos []
  where
    mousePos = Vector2 (width `div` 2) (height `div` 2)

    (Vector3 targetX targetY targetZ) = target
    (Vector3 hTargetX _ hTargetZ) = normalizeVector (Vector3 targetX 0 targetZ)

    angleV = - toDegree (asin targetY)
    angleH | hTargetZ >= 0 && hTargetX >= 0 = 360 - toDegree (asin hTargetZ)
           | hTargetZ >= 0                  = 180 + toDegree (asin hTargetZ)
           | hTargetX >= 0                  = toDegree (asin (-hTargetZ))
           | otherwise                      = 90 + toDegree (asin (-hTargetZ))

cameraOnKeyboard :: SpecialKey -> Camera -> Camera
cameraOnKeyboard KeyUp    c@Camera{..} =
    c{ cameraPos = cameraPos + fmap (*stepScale) cameraTarget }
cameraOnKeyboard KeyDown  c@Camera{..} =
    c{ cameraPos = cameraPos - fmap (*stepScale) cameraTarget }
cameraOnKeyboard KeyLeft  c@Camera{..} =
    c{ cameraPos = cameraPos + fmap (*stepScale) left }
  where
    left = normalizeVector ( cameraTarget * cameraUp )
cameraOnKeyboard KeyRight c@Camera{..} =
    c{ cameraPos = cameraPos + fmap (*stepScale) right }
  where
    right = normalizeVector ( cameraUp * cameraTarget )
cameraOnKeyboard _ camera = camera

cameraOnMouse :: Position -> Camera -> Camera
cameraOnMouse (Position x y) c@Camera{..} = cameraUpdate $
    c { cameraAngleH   = cameraAngleH + fromIntegral deltaX * rotationScale
      , cameraAngleV   = cameraAngleV + fromIntegral deltaY * rotationScale
      , cameraOnEdges  = catMaybes edges
      , cameraMousePos = Vector2 x y
      }
  where
    Vector2 deltaX deltaY = Vector2 x y - cameraMousePos

    edges :: [Maybe Edge]
    edges = map (uncurry wrapMaybe)
        [ (deltaX == 0 && x <= margin, LeftEdge)
        , (deltaX == 0 && x >= cameraWindowWidth - margin, RightEdge)
        , (deltaY == 0 && y <= margin, UpperEdge)
        , (deltaY == 0 && y >= cameraWindowHeight - margin, LowerEdge)
        ]

cameraOnRender :: Camera -> Camera
cameraOnRender c@Camera{..}
    | null cameraOnEdges = c
    | otherwise = cameraUpdate $
        c { cameraAngleH = angleH
          , cameraAngleV = angleV
          }
  where
    (angleH, angleV) =
        foldl moveTarget (cameraAngleH, cameraAngleV) cameraOnEdges

    moveTarget :: (GLfloat, GLfloat) -> Edge -> (GLfloat, GLfloat)
    moveTarget (h, v) UpperEdge = if v > -90
        then (h, v - edgeStep)
        else (h, v)
    moveTarget (h, v) LowerEdge = if v < 90
        then (h, v + edgeStep)
        else (h, v)
    moveTarget (h, v) LeftEdge  = (h - edgeStep, v)
    moveTarget (h, v) RightEdge = (h + edgeStep, v)

cameraUpdate :: Camera -> Camera
cameraUpdate c@Camera{..} = c { cameraTarget = target, cameraUp = up }
  where
    vaxis  = Vector3 0 1 0
    view   = normalizeVector $ rotateVector cameraAngleH (Vector3 1 0 0) vaxis
    haxis  = normalizeVector $ vaxis * view
    target = rotateVector cameraAngleV view haxis
    up     = normalizeVector $ target * haxis
