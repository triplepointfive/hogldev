{-# LANGUAGE RecordWildCards #-}
module ParticleSystem (
    ParticleSystem (..)
  , initParticleSystem
  , renderPS
) where

import           Foreign.Storable
import           Foreign.Ptr

import           Graphics.Rendering.OpenGL

import           Hogldev.RandomTexture as RT
import           Hogldev.Texture as T
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
    , currVB            :: !Int
    , currTFB           :: !Int
    , particleBuffer    :: ![BufferObject]
    , transformFeedback :: ![BufferObject]
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

    vPosition = AttribLocation 0
    vTextCoord = AttribLocation 1
    vNormals = AttribLocation 2
    vTangent = AttribLocation 3

    updateParticles :: IO ()
    updateParticles = undefined
        enablePSUpdateTechnique
        setTime
        setDeltaTimeMillis
        RT.textureBind (randTexture ps) RandomTexture(4)

        rasterizerDiscard $= Enabled

        bindBuffer ArrayBuffer $= Just (particleBuffer ps !! currVB ps)
        -- bindTransformFeedback

        vertexAttribArray vPosition $= Enabled
        vertexAttribArray vTextCoord $= Enabled
        vertexAttribArray vNormals $= Enabled
        vertexAttribArray vTangent $= Enabled

        vertexAttribPointer vPosition $=
            ( ToFloat
            , VertexArrayDescriptor 3 Float (fromIntegral vertexSize)
              (bufferOffset 0)
            )
        vertexAttribPointer vTextCoord $=
            ( ToFloat
            , VertexArrayDescriptor 2 Float (fromIntegral vertexSize)
              (bufferOffset (sizeOf (Vertex3 0 0 0 :: Vertex3 GLfloat)))
            )
        vertexAttribPointer vNormals $=
            ( ToFloat
            , VertexArrayDescriptor 3 Float (fromIntegral vertexSize)
              (bufferOffset (
                  sizeOf (Vertex3 0 0 0 :: Vertex3 GLfloat)
                + sizeOf (TexCoord2 0 0 :: TexCoord2 GLfloat))
              )
            )
        vertexAttribPointer vTangent $=
            ( ToFloat
            , VertexArrayDescriptor 3 Float (fromIntegral vertexSize)
              (bufferOffset (
                  sizeOf (Vertex3 0 0 0 :: Vertex3 GLfloat)    -- Position
                + sizeOf (TexCoord2 0 0 :: TexCoord2 GLfloat)  -- Texture
                + sizeOf (Vertex3 0 0 0 :: Vertex3 GLfloat))   -- Normal
              )
            )

        beginTransformFeedback Points

        if (isFirst ps)
          then drawA
            drawArrays Points 0 1
            -- isFirst = false
          else
            -- drawTransformFeedback

        endTransfromFeedback

        vertexAttribArray vPosition $= Disabled
        vertexAttribArray vTextCoord $= Disabled
        vertexAttribArray vNormals $= Disabled
        vertexAttribArray vTangent $= Disabled

    renderParticles :: IO ()
    renderParticles = do
        enableBillboardTechnique billboardTech
        setBillboardCameraPosition billboardTech cameraPos
        setBillboardTechniqueVP billboardTech vp
        T.textureBind (texture ps) (TextureUnit 0)

        rasterizerDiscard $= Disabled

        bindBuffer ArrayBuffer $= Just (particleBuffer ps !! currVB ps)

        vertexAttribArray vPosition $= Enabled

        vertexAttribPointer vPosition $=
            ( ToFloat
            , VertexArrayDescriptor 3 Float (fromIntegral vertexSize)
              (bufferOffset (sizeOf (0 :: GLfloat)))
            )

        -- drawTransformFeedback

        vertexAttribArray vPosition $= Disabled
