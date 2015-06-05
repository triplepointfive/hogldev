{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Hogldev.Camera (
    Camera(..)
  , cameraOnKeyboard
) where

import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLUT (SpecialKey(..))

import           Hogldev.Utils

stepScale = 0.1

data Camera = Camera
              { cameraPos    :: !(Vector3 GLfloat)
              , cameraTarget :: !(Vector3 GLfloat)
              , cameraUp     :: !(Vector3 GLfloat)
              }

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
