module ParticleSystem (
    ParticleSystem (..)
  , initParticleSystem
  , renderPS
) where

import           Graphics.Rendering.OpenGL

import           Hogldev.Texture
import           Hogldev.Math3D (Matrix4)
import           BillboardTechnique

type RandomTexture = Texture

data ParticleSystem =
    ParticleSystem
    { isFirst           :: !Bool
    , currVB            :: !BufferObject
    , currTFB           :: !BufferObject
    , particleBuffer    :: ![GLuint]
    , transformFeedback :: ![GLuint]
    , psUpdateTech      :: !PSUpdateTechnique
    , billboardTech     :: !BillboardTechnique
    , randTexture       :: !RandomTexture
    , texture           :: !Texture
    , time              :: !Int
    } deriving Show

initParticleSystem :: Vector3 GLfloat -> IO ()
initParticleSystem pos = undefined

renderPS :: Int -> Matrix4 -> Vector3 GLfloat -> IO ()
renderPS deltaTimeMillis vp cameraPos = undefined
