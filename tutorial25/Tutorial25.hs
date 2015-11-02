module Main where

import           Control.Monad (when)
import           Data.IORef
import           Data.Maybe (isNothing, fromJust)
import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLUT hiding (exit)
import           System.Exit (exitFailure, exitSuccess)

import           Hogldev.ShadowMapFBO
import           Hogldev.Texture
import           Hogldev.Pipeline (
                    Pipeline(..), getTrans,
                    PersProj(..)
                 )
import           Hogldev.Camera ( Camera(..), cameraOnKeyboard,
                    initCamera, cameraOnMouse, cameraOnRender
                 )

import           LightingTechnique
import           Mesh
import           ShadowMapTechnique

windowWidth = 1920
windowHeight = 1080

persProjection = PersProj
                 { persFOV   = 60
                 , persWidth = fromIntegral windowWidth
                 , persHeigh = fromIntegral windowHeight
                 , persZNear = 1
                 , persZFar  = 50
                 }

spotlightPos = Vector3 (-20.0) 20.0 1.0
spotlightDir = Vector3 1.0 (-1.0) 0.0
spotLights =
  [ DirectionalLight
    { dAmbientColor     = Vertex3 1 1 1
    , dAmbientIntensity = 0.1
    , dDiffuseIntensity = 0.9
    , dPosition         = Vertex3 (-20.0) 20.0 1.0
    , dConstant         = 1
    , dLinear           = 0.01
    , dExp              = 0
    , dDirection        = Vertex3 1.0 (-1.0) 0.0
    , dCutOff           = 20
    }
  ]

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

    shadowMapFBO <- initializeShadowMapFBO windowWidth windowHeight
    lightingEffect <- initLightingTechnique

    enableLightingTechnique lightingEffect
    setSpotLights lightingEffect 1 spotLights
    setLightingTextureUnit lightingEffect 0
    setLightingShadowMapTextureUnit lightingEffect 1

    shadowMapEffect <- initShadowMapTechnique

    pointerPosition $= mousePos

    quad <- loadMesh "assets/quad.obj"
    groundTex <- textureLoad "assets/test.png" Texture2D
    when (isNothing groundTex) exitFailure
    mesh <- loadMesh "assets/phoenix_ugv.md2"

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
                    worldInfo  = Vector3 0 0 1,
                    scaleInfo  = Vector3 10 10 10,
                    rotateInfo = Vector3 90 0 0,
                    persProj   = persProjection,
                    pipeCamera = camera
                }
            setLightingWorldMatrix lightingEffect $ getTrans
                WPipeline {
                    worldInfo  = Vector3 0 0 1,
                    scaleInfo  = Vector3 10 10 10,
                    rotateInfo = Vector3 90 0 0
                }
            setLightingLightWVPMatrix lightingEffect $ getTrans
                WVPPipeline {
                    worldInfo  = Vector3 0 0 1,
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

            renderMesh mesh

    shadowMapPass
    renderPass

    swapBuffers
