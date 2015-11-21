module Main where

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
import           Hogldev.Texture

import           BillboardList
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
    , diffuseDirection = Vertex3 1.0 0.0 0.0
    }

main :: IO ()
main = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBAMode, WithDepthBuffer]
    initialWindowSize $= Size windowWidth windowHeight
    initialWindowPosition $= Position 100 100
    createWindow "Tutorial 28"

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

    boxMesh <- loadMesh "assets/quad.obj"
    Just texture <- textureLoad "assets/bricks.jpg" Texture2D
    billboardList <- initBillboardList "assets/monster_hellknight.png"

    initializeGlutCallbacks boxMesh lightingEffect gScale cameraRef
        texture billboardList
    clearColor $= Color4 0 0 0 0

    mainLoop
  where
    newCamera = initCamera (Just
        (Vector3 0 1 (-1), Vector3 0 (-0.5) 1, Vector3 0 1 0)
        ) windowWidth windowHeight
    mousePos = Position (windowWidth `div` 2) (windowHeight `div` 2)

initializeGlutCallbacks :: Mesh
                        -> LightingTechnique
                        -> IORef GLfloat
                        -> IORef Camera
                        -> Texture
                        -> BillboardList
                        -> IO ()
initializeGlutCallbacks boxMesh lightingEffect gScale cameraRef texture billboardList = do
    displayCallback $=
        renderSceneCB boxMesh lightingEffect gScale cameraRef texture billboardList
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
              -> IORef GLfloat
              -> IORef Camera
              -> Texture
              -> BillboardList
              -> DisplayCallback
renderSceneCB boxMesh lightingEffect gScale cameraRef texture billboardList = do
    cameraRef $~! cameraOnRender
    gScaleVal <- readIORef gScale
    camera <- readIORef cameraRef

    clear [ColorBuffer, DepthBuffer]

    enableLightingTechnique lightingEffect

    textureBind texture (TextureUnit 0)

    setLightingWVP lightingEffect $ getTrans
        WVPPipeline {
            worldInfo  = Vector3 0 0 3,
            scaleInfo  = Vector3 20 20 1,
            rotateInfo = Vector3 (-90) 0 0,
            persProj   = persProjection,
            pipeCamera = camera
        }
    setLightingWorldMatrix lightingEffect $ getTrans
        WPipeline {
            worldInfo  = Vector3 0 0 3,
            scaleInfo  = Vector3 20 20 1,
            rotateInfo = Vector3 (-90) 0 0
        }

    renderMesh boxMesh

    render billboardList (vpTrans camera) (cameraPos camera)

    swapBuffers
  where
    vpTrans camera = getTrans
        VPPipeline {
            persProj   = persProjection,
            pipeCamera = camera
        }
