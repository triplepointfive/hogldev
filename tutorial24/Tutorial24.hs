module Main where

import           Data.IORef
import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLUT hiding (exit)
import           System.Exit (exitSuccess)

import           Hogldev.Pipeline (
                    Pipeline(..), getTrans,
                    PersProj(..)
                 )
import           Hogldev.Camera (
                    Camera(..), cameraOnKeyboard,
                    initCamera, cameraOnMouse, cameraOnRender
                 )
import           Mesh
import           ShadowMapTechnique
import           ShadowMapFBO

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
    initialDisplayMode $= [DoubleBuffered, RGBAMode, WithDepthBuffer]
    initialWindowSize $= Size windowWidth windowHeight
    initialWindowPosition $= Position 100 100
    createWindow "Tutorial 24"

    -- frontFace $= CW
    -- cullFace $= Just Back
    depthFunc $= Just Lequal

    gScale <- newIORef 0.0
    cameraRef <- newIORef newCamera

    shadowMapFBO <- initializeShadowMapFBO windowWidth windowHeight

    effect <- initShadowMapTechnique
    enableShadowMapTechnique effect

    pointerPosition $= mousePos

    mesh <- loadMesh "assets/phoenix_ugv.md2"
    quad <- loadMesh "assets/quad.obj"

    initializeGlutCallbacks mesh quad shadowMapFBO effect  gScale cameraRef
    clearColor $= Color4 0 0 0 0

    mainLoop
  where
    newCamera = initCamera Nothing windowWidth windowHeight
    mousePos = Position (windowWidth `div` 2) (windowHeight `div` 2)

initializeGlutCallbacks :: Mesh
                        -> Mesh
                        -> ShadowMapFBO
                        -> ShadowMapTechnique
                        -> IORef GLfloat
                        -> IORef Camera
                        -> IO ()
initializeGlutCallbacks mesh quad shadowMapFBO effect gScale cameraRef = do
    displayCallback $=
        renderSceneCB mesh quad shadowMapFBO effect gScale cameraRef
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
  gScale $~! (+ 0.01)
  cameraRef $~! cameraOnRender
  postRedisplay Nothing

renderSceneCB :: Mesh
              -> Mesh
              -> ShadowMapFBO
              -> ShadowMapTechnique
              -> IORef GLfloat
              -> IORef Camera
              -> DisplayCallback
renderSceneCB mesh quad shadowMapFBO effect gScale cameraRef = do
    cameraRef $~! cameraOnRender
    gScaleVal <- readIORef gScale
    camera <- readIORef cameraRef
    let shadowMapPass :: IO ()
        shadowMapPass = do
            bindForWriting shadowMapFBO
            clear [DepthBuffer]
            setShadowMapWVP effect $ getTrans
                WVPPipeline {
                    worldInfo  = Vector3 0 0 5,
                    scaleInfo  = Vector3 0.1 0.1 0.1,
                    rotateInfo = Vector3 (-90) gScaleVal 0,
                    persProj   = persProjection,
                    pipeCamera = initCamera (Just (Vector3 (-20) 20 5, Vector3 1 (-1) 0, Vector3 0 1 0)) 0 0
                }
            renderMesh mesh
            bindFramebuffer Framebuffer $= defaultFramebufferObject

        renderPass :: IO ()
        renderPass = do
            clear [ColorBuffer, DepthBuffer]
            setShadowMapTextureUnit effect 0
            bindForReading shadowMapFBO (TextureUnit 0)
            setShadowMapWVP effect $ getTrans
                WVPPipeline {
                    worldInfo  = Vector3 0 0 10,
                    scaleInfo  = Vector3 2 2 2,
                    rotateInfo = Vector3 0 0 0,
                    persProj   = persProjection,
                    pipeCamera = camera
                }
            renderMesh quad

    shadowMapPass
    renderPass

    swapBuffers
