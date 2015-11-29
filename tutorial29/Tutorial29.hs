module Main where

import           Control.Monad (forM_)
import           Data.IORef
import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLUT hiding (exit)
import           System.Exit (exitSuccess)

import           Hogldev.Pipeline (
                    Pipeline(..), getTrans,
                    PersProj(..)
                 )
import           Hogldev.Camera ( Camera(..), cameraOnKeyboard,
                    initCamera, cameraOnMouse, cameraOnRender
                 )

import           Hogldev.LightingTechnique
import           Mesh

import           SimpleColorTechnique
import           PickingTexture
import           PickingTechnique

windowWidth = 1680
windowHeight = 1050

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
    , ambientIntensity = 1.0
    , diffuseIntensity = 0.01
    , diffuseDirection = Vertex3 1.0 (-1.0) 0.0
    }


worldPos :: [(GLuint, Vector3 GLfloat)]
worldPos = zip [0..] [Vector3 (-10.0) 0.0 5.0, Vector3 10.0 0.0 5.0]

main :: IO ()
main = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBAMode, WithDepthBuffer]
    initialWindowSize $= Size windowWidth windowHeight
    initialWindowPosition $= Position 100 100
    createWindow "Tutorial 29"

    -- frontFace $= CW
    -- cullFace $= Just Back
    depthFunc $= Just Lequal

    gScale <- newIORef 0.0
    cameraRef <- newIORef newCamera

    lightingEffect <- initLightingTechnique
    enableLightingTechnique lightingEffect
    setLightingTextureUnit lightingEffect 0
    setDirectionalLight lightingEffect dirLight

    pointerPosition $= mousePos

    pickingTexture <- initPickingTexture windowWidth windowHeight
    pickingEffect <- initPickingTechnique
    simpleColorEffect <- initSimpleColorTechnique
    mesh <- loadMesh "assets/spider.obj"

    initializeGlutCallbacks mesh lightingEffect pickingEffect simpleColorEffect
        gScale cameraRef pickingTexture
    clearColor $= Color4 0 0 0 0

    mainLoop
  where
    newCamera = initCamera (Just
        (Vector3 0 5 (-22.0), Vector3 0 (-0.2) 1, Vector3 0 1 0)
        ) windowWidth windowHeight
    mousePos = Position (windowWidth `div` 2) (windowHeight `div` 2)

initializeGlutCallbacks :: Mesh
                        -> LightingTechnique
                        -> PickingTechnique
                        -> SimpleColorTechnique
                        -> IORef GLfloat
                        -> IORef Camera
                        -> PickingTexture
                        -> IO ()
initializeGlutCallbacks mesh lightingEffect pickingEffect simpleColorEffect gScale cameraRef pickingTexture = do
    displayCallback $=
        renderSceneCB mesh lightingEffect pickingEffect simpleColorEffect gScale cameraRef pickingTexture
    idleCallback    $= Just (idleCB gScale cameraRef)
    specialCallback $= Just (specialKeyboardCB cameraRef)
    keyboardCallback $= Just keyboardCB
    passiveMotionCallback $= Just (passiveMotionCB cameraRef)

keyboardCB :: KeyboardCallback
keyboardCB 'q' _ = exitSuccess
keyboardCB  _ _ = return ()

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
              -> PickingTechnique
              -> SimpleColorTechnique
              -> IORef GLfloat
              -> IORef Camera
              -> PickingTexture
              -> DisplayCallback
renderSceneCB mesh lightingEffect pickingEffect simpleColorEffect gScale cameraRef pickingTexture = do
    cameraRef $~! cameraOnRender
    gScaleVal <- readIORef gScale
    camera <- readIORef cameraRef

    let pickingPhase = do
            enableWriting pickingTexture

            clear [ColorBuffer, DepthBuffer]

            enablePickingTechnique pickingEffect

            forM_ worldPos $ \ (i, pos) -> do
                setPickingTechniqueWVP pickingEffect $ getTrans
                    WVPPipeline {
                        worldInfo  = pos,
                        scaleInfo  = Vector3 0.1 0.1 0.1,
                        rotateInfo = Vector3 (-90) 90 0,
                        persProj   = persProjection,
                        pipeCamera = camera
                    }

                setPickingObjectIndex pickingEffect i

                renderMesh mesh -- pickingEffect

            disableWriting pickingTexture

        renderPhase = do
            clear [ColorBuffer, DepthBuffer]

            enableLightingTechnique lightingEffect
            setEyeWorldPos lightingEffect (cameraPos camera)

            forM_ worldPos $ \ (_, pos) -> do
                setLightingWVP lightingEffect $ getTrans
                    WVPPipeline {
                        worldInfo  = pos,
                        scaleInfo  = Vector3 0.1 0.1 0.1,
                        rotateInfo = Vector3 (-90) 90 0,
                        persProj   = persProjection,
                        pipeCamera = camera
                    }
                setLightingWorldMatrix lightingEffect $ getTrans
                    WPipeline {
                        worldInfo  = pos,
                        scaleInfo  = Vector3 0.1 0.1 0.1,
                        rotateInfo = Vector3 (-90) 90 0
                    }
                renderMesh mesh

    pickingPhase
    renderPhase

    swapBuffers
  where
    vpTrans camera = getTrans
        VPPipeline {
            persProj   = persProjection,
            pipeCamera = camera
        }
