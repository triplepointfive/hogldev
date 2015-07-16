module Main where

import           Control.Monad (unless)
import           Data.IORef
import           Graphics.Rendering.OpenGL
import           Graphics.GLUtil
import           Graphics.UI.GLUT hiding (exit)
import           Foreign.Marshal.Array (withArray)
import           Foreign.Storable (sizeOf)
import           System.Exit (exitFailure)

import           Hogldev.Pipeline (
                    Pipeline(..), getTrans,
                    PersProj(..)
                 )
import           Hogldev.Utils (bufferOffset)
import           Hogldev.Camera (Camera(..), cameraOnKeyboard, initCamera)

windowWidth = 1024
windowHeight = 768

persProjection = PersProj
                 { persFOV   = 30
                 , persWidth = fromIntegral windowWidth
                 , persHeigh = fromIntegral windowHeight
                 , persZNear = 1
                 , persZFar  = 1000
                 }

main :: IO ()
main = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBAMode]
    initialWindowSize $= Size windowWidth windowHeight
    initialWindowPosition $= Position 100 100
    createWindow "Tutorial 14"

    vbo <- createVertexBuffer
    ibo <- createIndexBuffer
    gWorldLocation <- compileShaders
    gScale <- newIORef 0.0
    cameraRef <- newIORef newCamera

    initializeGlutCallbacks vbo ibo gWorldLocation gScale cameraRef
    clearColor $= Color4 0 0 0 0

    mainLoop
  where
    newCamera = initCamera Nothing windowWidth windowHeight

initializeGlutCallbacks :: BufferObject
                        -> BufferObject
                        -> UniformLocation
                        -> IORef GLfloat
                        -> IORef Camera
                        -> IO ()
initializeGlutCallbacks vbo ibo gWorldLocation gScale cameraRef = do
    displayCallback $= renderSceneCB vbo ibo gWorldLocation gScale cameraRef
    idleCallback    $= Just (idleCB gScale)
    specialCallback $= Just (specialKeyboardCB cameraRef)

specialKeyboardCB :: IORef Camera -> SpecialCallback
specialKeyboardCB cameraRef key _ = cameraRef $~! cameraOnKeyboard key

idleCB :: IORef GLfloat -> IdleCallback
idleCB gScale = do
  gScale $~! (+ 0.1)
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

    addShader shaderProgram "tutorial14/shader.vs" VertexShader
    addShader shaderProgram "tutorial14/shader.fs" FragmentShader

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
    uniformLocation shaderProgram "gWVP"

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
              -> IORef Camera
              -> DisplayCallback
renderSceneCB vbo ibo gWorldLocation gScale cameraRef = do
    clear [ColorBuffer]
    gScaleVal <- readIORef gScale
    camera <- readIORef cameraRef

    uniformMat gWorldLocation $= getTrans
        WVPPipeline {
            worldInfo   = Vector3 0 0 5,
            scaleInfo  = Vector3 1 1 1,
            rotateInfo = Vector3 0 gScaleVal 0,
            persProj   = persProjection,
            pipeCamera = camera
        }

    vertexAttribArray vPosition $= Enabled
    bindBuffer ArrayBuffer $= Just vbo
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset 0))
    bindBuffer ElementArrayBuffer $= Just ibo

    drawIndexedTris 4

    vertexAttribArray vPosition $= Disabled

    swapBuffers
 where
    vPosition = AttribLocation 0
