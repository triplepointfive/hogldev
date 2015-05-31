module Main where

import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLUT
import           Foreign.Marshal.Array (withArray)
import           Foreign.Ptr
import           Foreign.Storable (sizeOf)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

main :: IO ()
main = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBAMode]
    initialWindowSize $= Size 1024 768
    initialWindowPosition $= Position 100 100
    createWindow "Tutorial 03"

    vbo <- createVertexBuffer

    initializeGlutCallbacks vbo
    clearColor $= Color4 0 0 0 0

    mainLoop

initializeGlutCallbacks :: BufferObject -> IO ()
initializeGlutCallbacks vbo =
    displayCallback $= renderSceneCB vbo

createVertexBuffer :: IO BufferObject
createVertexBuffer = do
    vbo <- genObjectName
    bindBuffer ArrayBuffer $= Just vbo
    withArray vertices $ \ptr ->
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)
    return vbo
  where
    vertices :: [Vertex3 GLfloat]
    vertices = [ Vertex3 (-1) (-1) 0
               , Vertex3    1 (-1) 0
               , Vertex3    0    1 0 ]

    numVertices = length vertices
    vertexSize  = sizeOf (head vertices)
    size        = fromIntegral (numVertices * vertexSize)

renderSceneCB :: BufferObject -> IO ()
renderSceneCB vbo = do
    clear [ColorBuffer]

    vertexAttribArray vPosition $= Enabled
    bindBuffer ArrayBuffer $= Just vbo
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset 0))

    drawArrays Triangles 0 3

    vertexAttribArray vPosition $= Disabled

    swapBuffers
 where
    vPosition = AttribLocation 0
