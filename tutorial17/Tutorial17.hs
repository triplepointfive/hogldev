module Main where

import           Control.Monad (when, unless)
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
import           Hogldev.Technique

import           Data.Maybe (isNothing, fromJust)

import           Graphics.Rendering.OpenGL.Raw (
                     gl_TEXTURE_2D, glActiveTexture,
                     gl_TEXTURE0

                 )

import           LightingTechnique

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
    createWindow "Tutorial 17"

    frontFace $= CW
    cullFace $= Just Front

    vbo <- createVertexBuffer
    ibo <- createIndexBuffer

    texture <- textureLoad "assets/test.png" Texture2D
    when (isNothing texture) exitFailure

    gScale <- newIORef 0.0
    cameraRef <- newIORef newCamera
    dirLight <- newIORef directionLight

    effect <- initLightingTechnique
    enableTechnique (lightingProgram effect)
    setLightingTextureUnit effect 0

    pointerPosition $= mousePos

    initializeGlutCallbacks vbo ibo effect dirLight gScale cameraRef
        (fromJust texture)
    clearColor $= Color4 0 0 0 0

    mainLoop
  where
    newCamera = initCamera Nothing windowWidth windowHeight
    mousePos = Position (windowWidth `div` 2) (windowHeight `div` 2)
    directionLight = DirectionLight (Vertex3 1.0 1.0 1.0) 0.5

initializeGlutCallbacks :: BufferObject
                        -> BufferObject
                        -> LightingTechnique
                        -> IORef DirectionLight
                        -> IORef GLfloat
                        -> IORef Camera
                        -> Texture
                        -> IO ()
initializeGlutCallbacks vbo ibo effect dirLight gScale cameraRef texture = do
    displayCallback $=
        renderSceneCB vbo ibo effect dirLight gScale cameraRef texture
    idleCallback    $= Just (idleCB gScale cameraRef)
    specialCallback $= Just (specialKeyboardCB cameraRef)
    keyboardCallback $= Just (keyboardCB dirLight)
    passiveMotionCallback $= Just (passiveMotionCB cameraRef)

keyboardCB :: IORef DirectionLight -> KeyboardCallback
keyboardCB _ 'q' _ = exitSuccess
keyboardCB dirLight 'a' _ = do
    (DirectionLight color intensity) <- readIORef dirLight
    writeIORef dirLight (DirectionLight color (intensity + 0.05))
keyboardCB dirLight 's' _ = do
    (DirectionLight color intensity) <- readIORef dirLight
    writeIORef dirLight (DirectionLight color (intensity - 0.05))
keyboardCB _ _ _ = return ()

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

renderSceneCB :: BufferObject
              -> BufferObject
              -> LightingTechnique
              -> IORef DirectionLight
              -> IORef GLfloat
              -> IORef Camera
              -> Texture
              -> DisplayCallback
renderSceneCB vbo ibo effect dirLight gScale cameraRef texture = do
    cameraRef $~! cameraOnRender
    clear [ColorBuffer]
    gScaleVal <- readIORef gScale
    camera <- readIORef cameraRef
    directionLight <- readIORef dirLight

    setLightingWVP effect $ getTrans
        WVPPipeline {
            worldInfo  = Vector3 0 0 5,
            scaleInfo  = Vector3 1 1 1,
            rotateInfo = Vector3 0 gScaleVal 0,
            persProj   = persProjection,
            pipeCamera = camera
        }
    setDirectionalLight effect directionLight

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
