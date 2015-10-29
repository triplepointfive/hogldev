module Main where

import           Control.Monad (when)
import           Data.IORef
import           Data.Maybe (isNothing, fromJust)
import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLUT hiding (exit)
import           System.Exit (exitFailure, exitSuccess)

import           Hogldev.Texture
import           Hogldev.Pipeline (
                    Pipeline(..), getTrans,
                    PersProj(..)
                 )
import           Hogldev.Camera (
                    Camera(..), cameraOnKeyboard,
                    initCamera, cameraOnMouse, cameraOnRender
                 )

import           LightingTechnique
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

spotlightPos = Vector3 (-20.0) 20.0 1.0
spotlightDir = Vector3 1.0 (-1.0) 0.0

directionLight =
    DirectionLight
    { ambientColor     = Vertex3 1.0 1.0 1.0
    , ambientIntensity = 1.0
    , diffuseDirection = Vertex3 1.0 (-1) 0
    , diffuseIntensity = 0.01
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
    groundTex <- textureLoad "assets/test.png" Texture2D
    when (isNothing groundTex) exitFailure

    shadowMapEffect <- initShadowMapTechnique
    lightingEffect <- initLightingTechnique

    enableLightingTechnique lightingEffect
    setLightingTextureUnit lightingEffect 0
    setLightingShadowMapTextureUnit lightingEffect 1

    pointerPosition $= mousePos

    mesh <- loadMesh "assets/phoenix_ugv.md2"
    quad <- loadMesh "assets/quad.obj"

    initializeGlutCallbacks mesh quad shadowMapFBO lightingEffect shadowMapEffect  gScale cameraRef (fromJust groundTex)
    clearColor $= Color4 0 0 0 0

    mainLoop
  where
    newCamera = initCamera (Just
      (Vector3 3 8 (-10), Vector3 0 (-0.2) 1, Vector3 0 1 0)
      ) windowWidth windowHeight
    mousePos = Position (windowWidth `div` 2) (windowHeight `div` 2)

initializeGlutCallbacks :: Mesh
                        -> Mesh
                        -> ShadowMapFBO
                        -> LightingTechnique
                        -> ShadowMapTechnique
                        -> IORef GLfloat
                        -> IORef Camera
                        -> Texture
                        -> IO ()
initializeGlutCallbacks mesh quad shadowMapFBO lightingEffect shadowMapEffect gScale cameraRef texture = do
    displayCallback $=
        renderSceneCB mesh quad shadowMapFBO lightingEffect shadowMapEffect gScale cameraRef texture
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
              -> LightingTechnique
              -> ShadowMapTechnique
              -> IORef GLfloat
              -> IORef Camera
              -> Texture
              -> DisplayCallback
renderSceneCB mesh quad shadowMapFBO lightingEffect shadowMapEffect gScale cameraRef groundTex = do
    cameraRef $~! cameraOnRender
    gScaleVal <- readIORef gScale
    camera <- readIORef cameraRef
    let shadowMapPass :: IO ()
        shadowMapPass = do
            bindForWriting shadowMapFBO
            clear [DepthBuffer]
            enableShadowMapTechnique shadowMapEffect
            setShadowMapWVP shadowMapEffect $ getTrans
                WVPPipeline {
                    worldInfo  = Vector3 0 0 3,
                    scaleInfo  = Vector3 0.1 0.1 0.1,
                    rotateInfo = Vector3 (-90) gScaleVal 0,
                    persProj   = persProjection,
                    pipeCamera = initCamera (Just (spotlightPos, spotlightDir, Vector3 0 1 0)) windowWidth windowHeight
                }
            renderMesh mesh
            bindFramebuffer Framebuffer $= defaultFramebufferObject

        renderPass :: IO ()
        renderPass = do
            clear [ColorBuffer, DepthBuffer]
            enableLightingTechnique lightingEffect

            setEyeWorldPos lightingEffect (cameraPos camera)

            bindForReading shadowMapFBO (TextureUnit 1)

            setLightingWVP lightingEffect $ getTrans
                WVPPipeline {
                    worldInfo  = Vector3 0 0 10,
                    scaleInfo  = Vector3 10 10 10,
                    rotateInfo = Vector3 90 0 0,
                    persProj   = persProjection,
                    pipeCamera = camera
                }
            setLightingWorldMatrix lightingEffect $ getTrans
                WPipeline {
                    worldInfo  = Vector3 0 0 10,
                    scaleInfo  = Vector3 10 10 10,
                    rotateInfo = Vector3 90 0 0
                }
            setLightingLightWVPMatrix lightingEffect $ getTrans
                WVPPipeline {
                    worldInfo  = Vector3 0 0 10,
                    scaleInfo  = Vector3 10 10 10,
                    rotateInfo = Vector3 90 0 0,
                    persProj   = persProjection,
                    pipeCamera = initCamera (Just (spotlightPos, spotlightDir, Vector3 0 1 0)) windowWidth windowHeight
                }
            textureBind groundTex (TextureUnit 0)
            renderMesh quad

            setLightingWVP lightingEffect $ getTrans
                WVPPipeline {
                    worldInfo  = Vector3 0 0 3,
                    scaleInfo  = Vector3 0.1 0.1 0.1,
                    rotateInfo = Vector3 (-90) gScaleVal 0,
                    persProj   = persProjection,
                    pipeCamera = camera
                }
            setLightingWorldMatrix lightingEffect $ getTrans
                WPipeline {
                    worldInfo  = Vector3 0 0 3,
                    scaleInfo  = Vector3 0.1 0.1 0.1,
                    rotateInfo = Vector3 (-90) gScaleVal 0
                }
            setLightingLightWVPMatrix lightingEffect $ getTrans
                WVPPipeline {
                    worldInfo  = Vector3 0 0 3,
                    scaleInfo  = Vector3 0.1 0.1 0.1,
                    rotateInfo = Vector3 (-90) gScaleVal 0,
                    persProj   = persProjection,
                    pipeCamera = initCamera (Just (spotlightPos, spotlightDir, Vector3 0 1 0)) windowWidth windowHeight
                }

            setDirectionalLight lightingEffect directionLight
            renderMesh mesh

    shadowMapPass
    renderPass

    swapBuffers
