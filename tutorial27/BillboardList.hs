{-# LANGUAGE RecordWildCards #-}
module BillboardList (
    BillboardList (..)
  , initBillboardList
  , render
) where

import           Foreign.Marshal.Array (withArray)
import           Foreign.Storable (sizeOf)

import           Graphics.Rendering.OpenGL
import           Graphics.GLUtil (offset0)

import           Hogldev.Texture
import           Hogldev.Math3D (Matrix4)
import           BillboardTechnique

numRows = 10
numColumns = 10

data BillboardList =
    BillboardList
    { vb        :: !BufferObject
    , texture   :: !Texture
    , technique :: !BillboardTechnique
    } deriving Show

initBillboardList :: FilePath -> IO BillboardList
initBillboardList texFilename = do
    Just text <- textureLoad texFilename Texture2D
    vBuffer <- createPositionBuffer
    tech <- initBillboardTechnique
    return BillboardList
        { vb        = vBuffer
        , texture   = text
        , technique = tech
        }
  where
    createPositionBuffer :: IO BufferObject
    createPositionBuffer = do
        vbo <- genObjectName
        bindBuffer ArrayBuffer $= Just vbo
        withArray positions $ \ptr ->
            bufferData ArrayBuffer $= (size, ptr, StaticDraw)
        return vbo
      where
        positions :: [Vertex3 GLfloat]
        positions = map (fmap fromIntegral)
            [Vertex3 x 0 y| x <- [0..numRows - 1], y <- [0..numColumns - 1]]
        numVertices = length positions
        vertexSize  = sizeOf (head positions)
        size        = fromIntegral (numVertices * vertexSize)

render :: BillboardList -> Matrix4 -> Vector3 GLfloat -> IO ()
render BillboardList{..} vp cameraPos = do
    enableBillboardTechnique   technique
    setBillboardTechniqueVP    technique vp
    setBillboardCameraPosition technique cameraPos
    setBillboardTechniqueColorUnit technique 0

    textureBind texture (TextureUnit 0)

    vertexAttribArray vPosition $= Enabled

    bindBuffer ArrayBuffer $= Just vb
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 3 Float 0 offset0)

    drawArrays Points 0 (numRows * numColumns)

    vertexAttribArray vPosition $= Disabled
  where
    vPosition = AttribLocation 0
