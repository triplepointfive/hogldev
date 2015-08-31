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
import           Foreign.Marshal.Array (withArray)
import           Text.Printf (printf)
import           System.FilePath (splitFileName)

import qualified Data.Vector as V
import qualified Codec.Soten as S
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
  { entries  :: ![MeshEntry]
  , textures :: ![Texture]
  }

    -- indices :: [GLuint]
    -- indices = [ 0, 1, 2
              -- , 0, 2, 3
              -- ]
    -- vertices :: [TNVertex]
    -- vertices =
        -- [ TNVertex (Vertex3 (-10) (-2) (-10)) (TexCoord2 0 0) normal
        -- , TNVertex (Vertex3    10 (-2) (-10)) (TexCoord2 1 0) normal
        -- , TNVertex (Vertex3    10 (-2)    10) (TexCoord2 1 1) normal
        -- , TNVertex (Vertex3 (-10) (-2)    10) (TexCoord2 0 1) normal
        -- ]
    -- texture <- textureLoad "assets/test.png" Texture2D
    -- when (isNothing texture) exitFailure

vertexSize = sizeOf (TNVertex (Vertex3 0 0 0) (TexCoord2 0 0) (Vertex3 0 0 0))

loadMesh :: FilePath -> IO Mesh
loadMesh fileName = S.readModelFile fileName >>= either
    (error . printf "Error parsing '%s': '%s'" fileName)
    (initFromScene fileName)

initFromScene :: FilePath -> S.Scene -> IO Mesh
initFromScene fileName scene = do
    meshEntries <- V.mapM newMeshEntry (S._sceneMeshes scene)
    meshTextures <- initMaterials scene fileName
    return Mesh { entries = V.toList meshEntries, textures = meshTextures }

-- TODO: make it safe / add meaningfull fail message
initMaterials :: S.Scene -> FilePath -> IO [Texture]
initMaterials S.Scene{..} fileName = map fromJust <$> V.toList <$>
    V.mapM readTexture _sceneMaterials
  where
    readTexture S.Material{..} = textureLoad textureName Texture2D
      where
        textureName = V.foldl findTexturePropery
            "assets/white.png" _materialProperties

        findTexturePropery :: String -> S.MaterialProperty -> String
        findTexturePropery _ (S.MaterialTexture S.TextureTypeDiffuse name) =
            dir ++ name
        findTexturePropery name _ = name

    (dir, _) = splitFileName fileName

newMeshEntry :: S.Mesh -> IO MeshEntry
newMeshEntry mesh = initMeshEntry vertices indices
    where
      verticesCount = V.length (S._meshVertices mesh)
      meshTextures = if S.hasTextureCoords mesh 0
          then S._meshTextureCoords mesh
          else V.replicate verticesCount (S.V3 0 0 0)
      vertices = V.toList $ V.zipWith3 toVert
          (S._meshVertices mesh)
          (S._meshNormals mesh)
          meshTextures
      toVert :: S.V3 Float -> S.V3 Float -> S.V3 Float -> TNVertex
      toVert (S.V3 px py pz) (S.V3 nx ny nz) (S.V3 tx ty _) =
          TNVertex pos text norm
        where
          pos  = Vertex3 (realToFrac px) (realToFrac py) (realToFrac pz)
          text = TexCoord2 (realToFrac tx) (realToFrac ty)
          norm = Vertex3 (realToFrac nx) (realToFrac ny) (realToFrac nz)
      indices  = map fromIntegral $ V.toList $
          V.concatMap S._faceIndices (S._meshFaces mesh)

initMeshEntry :: [TNVertex] -> [GLuint] -> IO MeshEntry
initMeshEntry vertices indices = do
    vbo <- createVertexBuffer vertices
    ibo <- createIndexBuffer indices
    return MeshEntry
        { vb            = vbo
        , ib            = ibo
        , numIndices    = fromIntegral (length indices)
        , materialIndex = Nothing
        }

createVertexBuffer :: [TNVertex] -> IO BufferObject
createVertexBuffer vertices = do
    vbo <- genObjectName
    bindBuffer ArrayBuffer $= Just vbo
    withArray vertices $ \ptr ->
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)
    return vbo
  where
    numVertices = length vertices
    vertexSize  = sizeOf (head vertices)
    size        = fromIntegral (numVertices * vertexSize)

createIndexBuffer :: [GLuint] -> IO BufferObject
createIndexBuffer indices = do
    ibo <- genObjectName
    bindBuffer ElementArrayBuffer $= Just ibo
    withArray indices $ \ptr ->
        bufferData ElementArrayBuffer $= (size, ptr, StaticDraw)
    return ibo
  where
    numIndices = length indices
    indexSize  = sizeOf (head indices)
    size       = fromIntegral (numIndices * indexSize)

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
