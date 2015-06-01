module Main where

import           Control.Monad (unless)
import           Data.IORef
import           Graphics.Rendering.OpenGL
import           Graphics.GLUtil
import           Graphics.UI.GLUT hiding (exit)
import           Foreign.Marshal.Array (withArray)
import           Foreign.Ptr
import           Foreign.Storable (sizeOf)
import           System.Exit (exitFailure)

import           Hogldev.Math3D (translateMatrix)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

vertexShader = unlines
    [ "#version 330"
    , ""
    , "layout (location = 0) in vec3 Position;"
    , ""
    , "uniform mat4 gWorld;"
    , ""
    , "void main()"
    , "{"
    , "  gl_Position = gWorld * vec4(Position, 1.0);"
    , "}"
    ]

fragmentShader = unlines
    [ "#version 330"
    , ""
    , "out vec4 FragColor;"
    , ""
    , "void main()"
    , "{"
    , "  FragColor = vec4(1.0, 0.0, 0.0, 1.0);"
    , "}"
    ]

main :: IO ()
main = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBAMode]
    initialWindowSize $= Size 1024 768
    initialWindowPosition $= Position 100 100
    createWindow "Tutorial 06"

    vbo <- createVertexBuffer
    gWorldLocation <- compileShaders
    gScale <- newIORef 0.0

    initializeGlutCallbacks vbo gWorldLocation gScale
    clearColor $= Color4 0 0 0 0

    mainLoop

initializeGlutCallbacks :: BufferObject
                        -> UniformLocation
                        -> IORef GLfloat
                        -> IO ()
initializeGlutCallbacks vbo gWorldLocation gScale = do
    displayCallback $= renderSceneCB vbo gWorldLocation gScale
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
               , Vertex3    1 (-1) 0
               , Vertex3    0    1 0 ]

    numVertices = length vertices
    vertexSize  = sizeOf (head vertices)
    size        = fromIntegral (numVertices * vertexSize)

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
              -> UniformLocation
              -> IORef GLfloat
              -> DisplayCallback
renderSceneCB vbo gWorldLocation gScale = do
    clear [ColorBuffer]
    gScaleVal <- readIORef gScale

    gWorld <- translateMatrix (sin gScaleVal) 0 0

    uniformGLMat4 gWorldLocation $= gWorld

    vertexAttribArray vPosition $= Enabled
    bindBuffer ArrayBuffer $= Just vbo
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset 0))

    drawArrays Triangles 0 3

    vertexAttribArray vPosition $= Disabled

    swapBuffers
 where
    vPosition = AttribLocation 0
