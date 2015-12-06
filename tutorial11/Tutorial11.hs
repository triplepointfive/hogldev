module Main where

import           Control.Monad (unless)
import           Data.IORef
import           Graphics.Rendering.OpenGL
import           Graphics.GLUtil
import           Graphics.UI.GLUT hiding (exit)
import           Foreign.Marshal.Array (withArray)
import           Foreign.Storable (sizeOf)
import           System.Exit (exitFailure)

import           Hogldev.Pipeline (Pipeline(..), getTrans)

main :: IO ()
main = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBAMode]
    initialWindowSize $= Size 1024 768
    initialWindowPosition $= Position 100 100
    createWindow "Tutorial 11"

    vbo <- createVertexBuffer
    ibo <- createIndexBuffer
    gWorldLocation <- compileShaders
    gScale <- newIORef 0.0

    initializeGlutCallbacks vbo ibo gWorldLocation gScale
    clearColor $= Color4 0 0 0 0

    mainLoop

initializeGlutCallbacks :: BufferObject
                        -> BufferObject
                        -> UniformLocation
                        -> IORef GLfloat
                        -> IO ()
initializeGlutCallbacks vbo ibo gWorldLocation gScale = do
    displayCallback $= renderSceneCB vbo ibo gWorldLocation gScale
    idleCallback    $= Just (idleCB gScale)

idleCB :: IORef GLfloat -> IdleCallback
idleCB gScale = do
  gScale $~! (+ 0.001)
  postRedisplay Nothing

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
               , Vertex3    0 (-1) 1
               , Vertex3    1 (-1) 0
               , Vertex3    0    1 0 ]

    numVertices = length vertices
    vertexSize  = sizeOf (head vertices)
    size        = fromIntegral (numVertices * vertexSize)

createIndexBuffer :: IO BufferObject
createIndexBuffer = do
    ibo <- genObjectName
    bindBuffer ElementArrayBuffer $= Just ibo
    withArray indices $ \ptr ->
        bufferData ElementArrayBuffer $= (size, ptr, StaticDraw)
    return ibo
  where
    indices :: [GLuint]
    indices = [ 0, 3, 1
              , 1, 3, 2
              , 2, 3, 0
              , 0, 2, 1 ]
    numIndices = length indices
    indexSize  = sizeOf (head indices)
    size        = fromIntegral (numIndices * indexSize)

compileShaders :: IO UniformLocation
compileShaders = do
    shaderProgram <- createProgram

    addShader shaderProgram "tutorial11/shader.vs" VertexShader
    addShader shaderProgram "tutorial11/shader.fs" FragmentShader

    linkProgram shaderProgram
    linkStatus shaderProgram >>= \ status -> unless status $ do
        errorLog <- programInfoLog shaderProgram
        putStrLn $ "Error linking shader program: '" ++ errorLog ++ "'"
        exitFailure

    validateProgram shaderProgram
    validateStatus shaderProgram >>= \ status -> unless status $ do
        errorLog <- programInfoLog shaderProgram
        putStrLn $ "Invalid shader program: '" ++ errorLog ++ "'"
        exitFailure

    currentProgram $= Just shaderProgram
    uniformLocation shaderProgram "gWorld"

addShader :: Program -> FilePath -> ShaderType -> IO ()
addShader shaderProgram shaderFile shaderType = do
    shaderText <- readFile shaderFile
    shaderObj <- createShader shaderType
    shaderSourceBS shaderObj $= packUtf8 shaderText

    compileShader shaderObj
    compileStatus shaderObj >>= \ status -> unless status $ do
        errorLog <- shaderInfoLog shaderObj
        putStrLn ("Error compiling shader type " ++ show shaderType
            ++ ": '" ++ errorLog ++ "'")
        exitFailure

    attachShader shaderProgram shaderObj

renderSceneCB :: BufferObject
              -> BufferObject
              -> UniformLocation
              -> IORef GLfloat
              -> DisplayCallback
renderSceneCB vbo ibo gWorldLocation gScale = do
    clear [ColorBuffer]
    gScaleVal <- readIORef gScale

    uniformMat gWorldLocation $= getTrans
        WPipeline {
            scaleInfo = Vector3 (sin (gScaleVal * 0.1))
                     (sin (gScaleVal * 0.1))
                     (sin (gScaleVal * 0.1)),
            worldInfo = Vector3 (sin gScaleVal) 0 0,
            rotateInfo = Vector3 (sin gScaleVal * 90)
                     (sin gScaleVal * 90)
                     (sin gScaleVal * 90)
            }

    vertexAttribArray vPosition $= Enabled
    bindBuffer ArrayBuffer $= Just vbo
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 3 Float 0 offset0)
    bindBuffer ElementArrayBuffer $= Just ibo

    drawIndexedTris 4

    vertexAttribArray vPosition $= Disabled

    swapBuffers
 where
    vPosition = AttribLocation 0
