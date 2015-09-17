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
import           Hogldev.Technique

import           Hogldev.LightingTechnique
import           Mesh

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
    createWindow "Tutorial 22"

    -- frontFace $= CW
    -- cullFace $= Just Back
    depthFunc $= Just Lequal

    gScale <- newIORef 0.0
    cameraRef <- newIORef newCamera
    dirLight <- newIORef directionLight

    effect <- initLightingTechnique
    enableTechnique (lProgram effect)
    setLightingTextureUnit effect 0

    pointerPosition $= mousePos

    mesh <- loadMesh "assets/phoenix_ugv.md2"

    initializeGlutCallbacks mesh effect dirLight gScale cameraRef
    clearColor $= Color4 0 0 0 0

    mainLoop
  where
    newCamera = initCamera Nothing windowWidth windowHeight
    mousePos = Position (windowWidth `div` 2) (windowHeight `div` 2)
    directionLight =
        DirectionLight
        { ambientColor     = Vertex3 1.0 1.0 1.0
        , ambientIntensity = 1.0
        , diffuseDirection = Vertex3 1.0 0 1.0
        , diffuseIntensity = 0.01
        }

initializeGlutCallbacks :: Mesh
                        -> LightingTechnique
                        -> IORef DirectionLight
                        -> IORef GLfloat
                        -> IORef Camera
                        -> IO ()
initializeGlutCallbacks mesh effect dirLight gScale cameraRef = do
    displayCallback $=
        renderSceneCB mesh effect dirLight gScale cameraRef
    idleCallback    $= Just (idleCB gScale cameraRef)
    specialCallback $= Just (specialKeyboardCB cameraRef)
    keyboardCallback $= Just (keyboardCB dirLight)
    passiveMotionCallback $= Just (passiveMotionCB cameraRef)

keyboardCB :: IORef DirectionLight -> KeyboardCallback
keyboardCB _ 'q' _ = exitSuccess
keyboardCB dirLight 'a' _ =
    dirLight $~! changeAmbIntensity (+ 0.05)
keyboardCB dirLight 's' _ =
    dirLight $~! changeAmbIntensity (\ x -> x - 0.05)
keyboardCB dirLight 'z' _ =
    dirLight $~! changeDiffIntensity (+ 0.05)
keyboardCB dirLight 'x' _ =
    dirLight $~! changeDiffIntensity (\ x -> x - 0.05)
keyboardCB _ _ _ = return ()

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
              -> LightingTechnique
              -> IORef DirectionLight
              -> IORef GLfloat
              -> IORef Camera
              -> DisplayCallback
renderSceneCB mesh effect dirLight gScale cameraRef = do
    cameraRef $~! cameraOnRender
    clear [ColorBuffer, DepthBuffer]
    gScaleVal <- readIORef gScale
    camera <- readIORef cameraRef
    directionLight <- readIORef dirLight

    setLightingWVP effect $ getTrans
        WVPPipeline {
            worldInfo  = Vector3 0 0 5,
            scaleInfo  = Vector3 0.1 0.1 0.1,
            rotateInfo = Vector3 0 gScaleVal 0,
            persProj   = persProjection,
            pipeCamera = camera
        }
    setLightingWorldMatrix effect $ getTrans
        WPipeline {
            worldInfo  = Vector3 0 0 5,
            scaleInfo  = Vector3 0.1 0.1 0.1,
            rotateInfo = Vector3 0 gScaleVal 0
        }
    setDirectionalLight effect directionLight

    setEyeWorldPos effect (cameraPos camera)
    setMatSpecularPower effect 0
    setMaterialSpecularIntensity effect 0

    renderMesh mesh

    swapBuffers
