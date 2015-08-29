{-# LANGUAGE RecordWildCards #-}
module Mesh (
    Mesh
  , loadMesh
  , renderMesh
) where

import           Control.Monad (when)
import           Data.Foldable (forM_)
import           Data.Maybe (fromJust, isJust)
import           Foreign.Storable (sizeOf)

import           Graphics.GLUtil
import           Graphics.Rendering.OpenGL

import           Hogldev.Texture
import           Hogldev.Vertex (TNVertex(..))
import           Hogldev.Utils (bufferOffset)

data MeshEntry = MeshEntry
  { vb            :: !BufferObject
  , ib            :: !BufferObject
  , numIndices    :: !GLint
  , materialIndex :: !(Maybe Int)
  }

data Mesh = Mesh
  { entries :: [MeshEntry]
  , textures :: [Texture]
  }

vertexSize = sizeOf (TNVertex (Vertex3 0 0 0) (TexCoord2 0 0) (Vertex3 0 0 0))

loadMesh :: FilePath -> IO Mesh
loadMesh fileName = undefined

renderMesh :: Mesh -> IO ()
renderMesh Mesh{..} = do
    vertexAttribArray vPosition $= Enabled
    vertexAttribArray vTextCoord $= Enabled
    vertexAttribArray vNormals $= Enabled

    forM_ entries renderEntry

    vertexAttribArray vPosition $= Disabled
    vertexAttribArray vTextCoord $= Disabled
    vertexAttribArray vNormals $= Disabled

  where
    vPosition = AttribLocation 0
    vTextCoord = AttribLocation 1
    vNormals = AttribLocation 2

    renderEntry :: MeshEntry -> IO ()
    renderEntry MeshEntry{..} = do
        bindBuffer ArrayBuffer $= Just vb

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

        bindBuffer ElementArrayBuffer $= Just ib

        when (isJust materialIndex) $
          textureBind (textures !! fromJust materialIndex)(TextureUnit 0)

        drawIndexedTris (numIndices `div` 3)
