module Main where

import           Control.Monad (unless)
import           Data.IORef
import           Graphics.Rendering.OpenGL
import           Graphics.GLUtil
import           Graphics.UI.GLUT hiding (exit)
import           Foreign.Marshal.Array (withArray)
import           Foreign.Storable (sizeOf)
import           System.Exit (exitFailure)

import           Hogldev.Pipeline (Pipeline(..), getWPTrans, initPipeline)
import           Hogldev.Utils (PersProj(..), bufferOffset)

windowWidth = 1024
windowHeight = 768

persProjection = PersProj
                 { persFOV   = 30
                 , persWidth = fromIntegral windowWidth
                 , persHeigh = fromIntegral windowHeight
                 , persZNear = 1
                 , persZFar  = 1000
                 }

vertexShader = unlines
    [ "#version 330"
    , ""
    , "layout (location = 0) in vec3 Position;"
    , ""
    , "uniform mat4 gWorld;"
    , ""
    , "out vec4 Color;"
    , ""
    , "void main()"
    , "{"
    , "  gl_Position = gWorld * vec4(Position, 1.0);"
    , "  Color = vec4(clamp(Position, 0.0, 1.0), 1.0);"
    , "}"
    ]

fragmentShader = unlines
    [ "#version 330"
    , ""
    , "in vec4 Color;"
    , ""
    , "out vec4 FragColor;"
    , ""
    , "void main()"
    , "{"
    , "  FragColor = Color;"
    , "}"
    ]

main :: IO ()
main = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBAMode]
    initialWindowSize $= Size windowWidth windowHeight
    initialWindowPosition $= Position 100 100
    createWindow "Tutorial 12"

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

    addShader shaderProgram vertexShader VertexShader
    addShader shaderProgram fragmentShader FragmentShader

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

addShader :: Program -> String -> ShaderType -> IO ()
addShader shaderProgram shaderText shaderType = do
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

    uniformMat gWorldLocation $= getWPTrans
        ( initPipeline {
            worldPos   = (Vector3 0 0 5),
            rotateInfo = (Vector3 0 gScaleVal 0),
            persProj   = persProjection
            }
        )

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
