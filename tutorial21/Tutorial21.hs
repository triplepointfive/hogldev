module Main where

import           Control.Monad (when)
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
import           Hogldev.Vertex (TNVertex(..))
import           Hogldev.Technique

import           Data.Maybe (isNothing, fromJust)

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
    createWindow "Tutorial 21"

    frontFace $= CW
    cullFace $= Just Front

    vbo <- createVertexBuffer
    ibo <- createIndexBuffer indices

    texture <- textureLoad "assets/test.png" Texture2D
    when (isNothing texture) exitFailure

    gScale <- newIORef 0.0
    cameraRef <- newIORef newCamera
    dirLight <- newIORef directionLight

    effect <- initLightingTechnique
    enableTechnique (lProgram effect)
    setLightingTextureUnit effect 0

    pointerPosition $= mousePos

    initializeGlutCallbacks vbo ibo effect dirLight gScale cameraRef
        (fromJust texture)
    clearColor $= Color4 0 0 0 0

    mainLoop
  where
    newCamera = initCamera Nothing windowWidth windowHeight
    mousePos = Position (windowWidth `div` 2) (windowHeight `div` 2)
    directionLight =
        DirectionLight
        { ambientColor     = Vertex3 1.0 1.0 1.0
        , ambientIntensity = 0.0
        , diffuseDirection = Vertex3 1.0 (-1.0) 0.0
        , diffuseIntensity = 0.01
        }
    indices :: [GLuint]
    indices = [ 0, 1, 2
              , 0, 2, 3
              ]

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

createVertexBuffer :: IO BufferObject
createVertexBuffer = do
    vbo <- genObjectName
    bindBuffer ArrayBuffer $= Just vbo
    withArray vertices $ \ptr ->
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)
    return vbo
  where
    vertices :: [TNVertex]
    vertices =
        [ TNVertex (Vertex3 (-10) (-2) (-10)) (TexCoord2 0 0) normal
        , TNVertex (Vertex3    10 (-2) (-10)) (TexCoord2 1 0) normal
        , TNVertex (Vertex3    10 (-2)    10) (TexCoord2 1 1) normal
        , TNVertex (Vertex3 (-10) (-2)    10) (TexCoord2 0 1) normal
        ]

    numVertices = length vertices
    vertexSize  = sizeOf (head vertices)
    size        = fromIntegral (numVertices * vertexSize)
    normal      = Vertex3 0 1 0

createIndexBuffer :: [GLuint] -> IO BufferObject
createIndexBuffer indices = do
    ibo <- genObjectName
    bindBuffer ElementArrayBuffer $= Just ibo
    withArray indices $ \ptr ->
        bufferData ElementArrayBuffer $= (size, ptr, StaticDraw)
    return ibo
  where
    numIndices = length indices
    indexSize  = sizeOf (head indices)
    size       = fromIntegral (numIndices * indexSize)

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


    let spotLights =
          [ DirectionalLight
            { dAmbientColor     = Vertex3 0 1 1
            , dAmbientIntensity = 0
            , dDiffuseIntensity = 0.9
            , dPosition         = vecToVer (cameraPos camera)
            , dConstant         = 1
            , dLinear           = 0.1
            , dExp              = 0
            , dDirection        = vecToVer (cameraTarget camera)
            , dCutOff           = 10
            }
          , DirectionalLight
            { dAmbientColor     = Vertex3 1 1 1
            , dAmbientIntensity = 0
            , dDiffuseIntensity = 0.9
            , dPosition         = Vertex3 5 3 10
            , dConstant         = 1
            , dLinear           = 0.1
            , dExp              = 0
            , dDirection        = Vertex3 0 (-1) 0
            , dCutOff           = 20
            }
          ]
        pointLights =
          [ PointLight
            { pAmbientColor     = Vertex3 1 0.5 0
            , pAmbientIntensity = 0
            , pDiffuseIntensity = 0.25
            , pPosition         = Vertex3 3 1 ((cos gScaleVal + 1) * 5)
            , pConstant         = 1
            , pLinear           = 0.1
            , pExp              = 0
            }
          , PointLight
            { pAmbientColor     = Vertex3 0 0.5 1
            , pAmbientIntensity = 0
            , pDiffuseIntensity = 0.25
            , pPosition         = Vertex3 7 1 ((sin gScaleVal + 1) * 5)
            , pConstant         = 1
            , pLinear           = 0.1
            , pExp              = 0
            }
          ]

    setPointLights effect 2 pointLights
    setSpotLights effect 2 spotLights

    setLightingWVP effect $ getTrans
        WVPPipeline {
            worldInfo  = Vector3 0 0 5,
            scaleInfo  = Vector3 1 1 1,
            rotateInfo = Vector3 0 gScaleVal 0,
            persProj   = persProjection,
            pipeCamera = camera
        }
    setLightingWorldMatrix effect $ getTrans
        WPipeline {
            worldInfo  = Vector3 0 0 5,
            scaleInfo  = Vector3 1 1 1,
            rotateInfo = Vector3 0 gScaleVal 0
        }
    setDirectionalLight effect directionLight

    setEyeWorldPos effect (cameraPos camera)
    setMatSpecularPower effect 32
    setMaterialSpecularIntensity effect 1

    vertexAttribArray vPosition $= Enabled
    vertexAttribArray vTextCoord $= Enabled
    vertexAttribArray vNormals $= Enabled

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
    vertexAttribPointer vNormals $=
        ( ToFloat
        , VertexArrayDescriptor 3 Float (fromIntegral vertexSize)
          (bufferOffset (
              sizeOf (Vertex3 0 0 0 :: Vertex3 GLfloat)
            + sizeOf (TexCoord2 0 0 :: TexCoord2 GLfloat))
          )
        )

    bindBuffer ElementArrayBuffer $= Just ibo

    textureBind texture (TextureUnit 0)
    drawIndexedTris 2

    vertexAttribArray vPosition $= Disabled
    vertexAttribArray vTextCoord $= Disabled
    vertexAttribArray vNormals $= Disabled

    swapBuffers
 where
    vPosition = AttribLocation 0
    vTextCoord = AttribLocation 1
    vNormals = AttribLocation 2
    vertexSize = sizeOf $
        TNVertex (Vertex3 0 0 0) (TexCoord2 0 0) (Vertex3 0 0 0)
    vecToVer (Vector3 x y z) = Vertex3 x y z
