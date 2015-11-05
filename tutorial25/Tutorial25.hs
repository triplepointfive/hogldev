module Main where

import           Control.Monad (when)
import           Data.IORef
import           Data.Maybe (isNothing, fromJust)
import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLUT hiding (exit)
import           System.Exit (exitFailure, exitSuccess)

import           Hogldev.Pipeline (
                    Pipeline(..), getTrans,
                    PersProj(..)
                 )
import           Hogldev.Camera ( Camera(..), cameraOnKeyboard,
                    initCamera, cameraOnMouse, cameraOnRender
                 )
import           Hogldev.CubemapTexture (CubeMapFilenames(..))

import           Skybox
import           Hogldev.LightingTechnique
import           Mesh

windowWidth = 1920
windowHeight = 1080

persProjection = PersProj
                 { persFOV   = 60
                 , persWidth = fromIntegral windowWidth
                 , persHeigh = fromIntegral windowHeight
                 , persZNear = 1
                 , persZFar  = 100
                 }

dirLight :: DirectionLight
dirLight = DirectionLight
    { ambientColor     = Vertex3 1 1 1
    , ambientIntensity = 0.2
    , diffuseIntensity = 0.8
    , diffuseDirection = Vertex3 1.0 (-1.0) 0.0
    }

main :: IO ()
main = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBAMode, WithDepthBuffer]
    initialWindowSize $= Size windowWidth windowHeight
    initialWindowPosition $= Position 100 100
    createWindow "Tutorial 25"

    -- frontFace $= CW
    -- cullFace $= Just Back
    depthFunc $= Just Lequal

    gScale <- newIORef 0.0
    cameraRef <- newIORef newCamera

    lightingEffect <- initLightingTechnique
    enableLightingTechnique lightingEffect
    -- Set color texture unit.
    setDirectionalLight lightingEffect dirLight
    setLightingTextureUnit lightingEffect 0

    pointerPosition $= mousePos

    quad <- loadMesh "assets/quad.obj"
    tankMesh <- loadMesh "assets/phoenix_ugv.md2"
    skyBox <- initSkybox persProjection cubeMapFilenames

    initializeGlutCallbacks tankMesh quad lightingEffect gScale cameraRef skyBox
    clearColor $= Color4 0 0 0 0

    mainLoop
  where
    newCamera = initCamera (Just
        (Vector3 0 1 (-20), Vector3 0 0 1, Vector3 0 1 0)
        ) windowWidth windowHeight
    mousePos = Position (windowWidth `div` 2) (windowHeight `div` 2)

    cubeMapFilenames = CubeMapFilenames
        { directory    = "."
        , posXFilename = "../Content/sp3right.jpg"
        , negXFilename = "../Content/sp3left.jpg"
        , posYFilename = "../Content/sp3top.jpg"
        , negYFilename = "../Content/sp3bot.jpg"
        , posZFilename = "../Content/sp3front.jpg"
        , negZFilename = "../Content/sp3back.jpg"
        }

initializeGlutCallbacks :: Mesh
                        -> Mesh
                        -> LightingTechnique
                        -> IORef GLfloat
                        -> IORef Camera
                        -> Skybox
                        -> IO ()
initializeGlutCallbacks tankMesh quad lightingEffect gScale cameraRef texture = do
    displayCallback $=
        renderSceneCB tankMesh quad lightingEffect gScale cameraRef texture
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
              -> LightingTechnique
              -> IORef GLfloat
              -> IORef Camera
              -> Skybox
              -> DisplayCallback
renderSceneCB tankMesh quad lightingEffect gScale cameraRef skyBox = do
    cameraRef $~! cameraOnRender
    gScaleVal <- readIORef gScale
    camera <- readIORef cameraRef

    clear [ColorBuffer, DepthBuffer]

    enableLightingTechnique lightingEffect

    setLightingWVP lightingEffect $ getTrans
        WVPPipeline {
            worldInfo  = Vector3 0 (-5) 3,
            scaleInfo  = Vector3 0.1 0.1 0.1,
            rotateInfo = Vector3 90 0 0,
            persProj   = persProjection,
            pipeCamera = camera
        }
    setLightingWorldMatrix lightingEffect $ getTrans
        WPipeline {
            worldInfo  = Vector3 0 (-5) 3,
            scaleInfo  = Vector3 0.1 0.1 0.1,
            rotateInfo = Vector3 90 0 0
        }

    skyboxRender skyBox camera
    renderMesh tankMesh

    swapBuffers
