module Main where

import           Control.Monad (unless)
import           Data.IORef
import           Graphics.Rendering.OpenGL
import           Graphics.GLUtil
import           Graphics.UI.GLUT hiding (exit)
import           Foreign.Marshal.Array (withArray)
import           Foreign.Storable (sizeOf)
import           System.Exit (exitFailure, exitSuccess)

import           Hogldev.Pipeline (
                    Pipeline(..), getTrans,
                    PersProj(..)
                 )
import           Hogldev.Utils (bufferOffset)
import           Hogldev.Camera (
                    Camera(..), cameraOnKeyboard,
                    initCamera, cameraOnMouse, cameraOnRender
                 )
import           Hogldev.Texture
import           Hogldev.Vertex (TexturedVertex(..))

import           Control.Monad (when)
import           Data.Maybe (isNothing, fromJust)

import           Graphics.Rendering.OpenGL.Raw (
                     gl_TEXTURE_2D, glUniform1i, glActiveTexture,
                     gl_TEXTURE0

                 )

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
    , "layout (location = 1) in vec2 TexCoord;"
    , ""
    , "uniform mat4 gWVP;"
    , ""
    , "out vec2 TexCoord0;"
    , ""
    , "void main()"
    , "{"
    , "  gl_Position = gWVP * vec4(Position, 1.0);"
    , "  TexCoord0 = TexCoord;"
    , "}"
    ]

fragmentShader = unlines
    [ "#version 330"
    , ""
    , "in vec2 TexCoord0;"
    , ""
    , "uniform sampler2D gSampler;"
    , ""
    , "void main()"
    , "{"
    , "  gl_FragColor = texture2D(gSampler, TexCoord0);"
    , "}"
    ]

main :: IO ()
main = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBAMode]
    initialWindowSize $= Size windowWidth windowHeight
    initialWindowPosition $= Position 100 100
    createWindow "Tutorial 16"

    frontFace $= CW
    cullFace $= Just Front

    vbo <- createVertexBuffer
    ibo <- createIndexBuffer
    (gWVPLocation, UniformLocation gSamplerLocation) <- compileShaders

    texture <- textureLoad "assets/test.png" Texture2D
    when (isNothing texture) exitFailure

    glUniform1i gSamplerLocation 0
    gScale <- newIORef 0.0
    cameraRef <- newIORef newCamera
    pointerPosition $= mousePos

    initializeGlutCallbacks vbo ibo gWVPLocation gScale cameraRef
        (fromJust texture)
    clearColor $= Color4 0 0 0 0

    mainLoop
  where
    newCamera = initCamera Nothing windowWidth windowHeight
    mousePos = Position (windowWidth `div` 2) (windowHeight `div` 2)

initializeGlutCallbacks :: BufferObject
                        -> BufferObject
                        -> UniformLocation
                        -> IORef GLfloat
                        -> IORef Camera
                        -> Texture
                        -> IO ()
initializeGlutCallbacks vbo ibo gWVPLocation gScale cameraRef texture = do
    displayCallback $=
        renderSceneCB vbo ibo gWVPLocation gScale cameraRef texture
    idleCallback    $= Just (idleCB gScale cameraRef)
    specialCallback $= Just (specialKeyboardCB cameraRef)
    keyboardCallback $= Just keyboardCB
    passiveMotionCallback $= Just (passiveMotionCB cameraRef)

keyboardCB :: KeyboardCallback
keyboardCB 'q' _ = exitSuccess
keyboardCB _ _ = return ()

specialKeyboardCB :: IORef Camera -> SpecialCallback
specialKeyboardCB cameraRef key _ = cameraRef $~! cameraOnKeyboard key

passiveMotionCB :: IORef Camera -> MotionCallback
passiveMotionCB cameraRef position = cameraRef $~! cameraOnMouse position

idleCB :: IORef GLfloat -> IORef Camera -> IdleCallback
idleCB gScale cameraRef = do
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
    vertices :: [TexturedVertex]
    vertices = [ TexturedVertex (Vertex3 (-1) (-1) 0) (TexCoord2   0 0)
               , TexturedVertex (Vertex3    0 (-1) 1) (TexCoord2 0.5 0)
               , TexturedVertex (Vertex3    1 (-1) 0) (TexCoord2   1 0)
               , TexturedVertex (Vertex3    0    1 0) (TexCoord2 0.5 1)]

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
              , 1, 2, 0 ]
    numIndices = length indices
    indexSize  = sizeOf (head indices)
    size        = fromIntegral (numIndices * indexSize)

compileShaders :: IO (UniformLocation, UniformLocation)
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

    gWVPLocation <- uniformLocation shaderProgram "gWVP"
    gSamplerLocation <- uniformLocation shaderProgram "gSampler"

    currentProgram $= Just shaderProgram

    return (gWVPLocation, gSamplerLocation)

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
              -> IORef Camera
              -> Texture
              -> DisplayCallback
renderSceneCB vbo ibo gWVPLocation gScale cameraRef texture = do
    cameraRef $~! cameraOnRender
    clear [ColorBuffer]
    gScaleVal <- readIORef gScale
    camera <- readIORef cameraRef

    uniformMat gWVPLocation $= getTrans
        WVPPipeline {
            worldInfo  = Vector3 0 0 5,
            scaleInfo  = Vector3 1 1 1,
            rotateInfo = Vector3 0 gScaleVal 0,
            persProj   = persProjection,
            pipeCamera = camera
        }

    vertexAttribArray vPosition $= Enabled
    vertexAttribArray vTextCoord $= Enabled

    bindBuffer ArrayBuffer $= Just vbo

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

    bindBuffer ElementArrayBuffer $= Just ibo

    glActiveTexture gl_TEXTURE0
    textureBind texture (TextureUnit 0)
    drawIndexedTris 4

    vertexAttribArray vPosition $= Disabled
    vertexAttribArray vTextCoord $= Disabled

    swapBuffers
 where
    vPosition = AttribLocation 0
    vTextCoord = AttribLocation 1
    vertexSize = sizeOf (TexturedVertex (Vertex3 0 0 0) (TexCoord2 0 0))
