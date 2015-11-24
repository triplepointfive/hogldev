{-# LANGUAGE RecordWildCards #-}
module ParticleSystem (
    ParticleSystem (..)
  , initParticleSystem
  , renderPS
) where

import           Foreign.Storable
import           Foreign.Ptr

import           Graphics.Rendering.OpenGL

import           Hogldev.RandomTexture
import           Hogldev.Math3D (Matrix4)
import           BillboardTechnique
import           PSUpdateTechnique

data Particle =
    Particle
    { pType           :: !GLfloat
    , pPos            :: !(Vertex3 GLfloat)
    , pVel            :: !(Vertex3 GLfloat)
    , pLifetimeMillis :: !GLfloat
    } deriving Show

instance Storable Particle where
    sizeOf ~(Particle t p v lm) = sizeOf t + sizeOf p + sizeOf v + sizeOf lm
    alignment ~(Particle t _ _ _) = alignment t
    peek ptr = do
        t <- peek (castPtr ptr)
        p <- peekByteOff (castPtr ptr) (sizeOf t)
        v <- peekByteOff (castPtr ptr) (sizeOf t + sizeOf p)
        lm <- peekByteOff (castPtr ptr) (sizeOf t + sizeOf p + sizeOf v)
        return (Particle t p v lm)
    poke ptr (Particle t p v lm) = do
        poke (castPtr ptr) t
        pokeByteOff (castPtr ptr) (sizeOf t) p
        pokeByteOff (castPtr ptr) (sizeOf t + sizeOf p) v
        pokeByteOff (castPtr ptr) (sizeOf t + sizeOf p + sizeOf v) lm

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

initParticleSystem :: Vector3 GLfloat -> IO ParticleSystem
initParticleSystem pos = undefined

renderPS :: Int -> Matrix4 -> Vector3 GLfloat -> ParticleSystem
         -> IO ParticleSystem
renderPS deltaTimeMillis vp cameraPos particleSystem = do
    updateParticles
    renderParticles
    return ps
    -- Switch buffers
  where
    ps = particalSystem { time = time particleSystem }

    updateParticles :: IO ()
    updateParticles = undefined

    renderParticles :: IO ()
    renderParticles = undefined

