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
import           Hogldev.Utils (bufferOffset, normalizeVertex)
import           Hogldev.Camera (
                    Camera(..), cameraOnKeyboard,
                    initCamera, cameraOnMouse, cameraOnRender
                 )
import           Hogldev.Texture
import           Hogldev.Vertex (TexturedVertex(..), TNVertex(..))
import           Hogldev.Technique

import           Data.Maybe (isNothing, fromJust)
import           Data.List.Split (chunksOf)
import           Data.List (genericIndex, genericSplitAt)

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
    createWindow "Tutorial 19"

    frontFace $= CW
    cullFace $= Just Front

    vbo <- createVertexBuffer indices
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
        , ambientIntensity = 0.00
        , diffuseDirection = Vertex3 1.0 0.0 0.0
        , diffuseIntensity = 0.75
        }
    indices :: [GLuint]
    indices = [ 0, 3, 1
              , 1, 3, 2
              , 2, 3, 0
              , 1, 2, 0 ]

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
  gScale $~! (+ 0.1)
  cameraRef $~! cameraOnRender
  postRedisplay Nothing

createVertexBuffer :: [GLuint] -> IO BufferObject
createVertexBuffer indices = do
    vbo <- genObjectName
    bindBuffer ArrayBuffer $= Just vbo
    withArray vertices $ \ptr ->
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)
    return vbo
  where
    vertices :: [TNVertex]
    vertices = calcNormals indices
        [ TexturedVertex (Vertex3 (-1) (-1) 0) (TexCoord2   0 0)
        , TexturedVertex (Vertex3    0 (-1) 1) (TexCoord2 0.5 0)
        , TexturedVertex (Vertex3    1 (-1) 0) (TexCoord2   1 0)
        , TexturedVertex (Vertex3    0    1 0) (TexCoord2 0.5 1)]

    numVertices = length vertices
    vertexSize  = sizeOf (head vertices)
    size        = fromIntegral (numVertices * vertexSize)

calcNormals :: [GLuint] -> [TexturedVertex] -> [TNVertex]
calcNormals indices vertices = zipWith addNormal vertices normals
  where
    addNormal :: TexturedVertex -> Vertex3 GLfloat -> TNVertex
    addNormal (TexturedVertex v t) n = TNVertex v t (normalizeVertex n)

    normals :: [Vertex3 GLfloat]
    normals = foldl triangleNormal nullNormals (chunksOf 3 indices)
      where
        nullNormals = replicate (length indices `div` 3) (Vertex3 0 1 0)

    triangleNormal :: [Vertex3 GLfloat] -> [GLuint] -> [Vertex3 GLfloat]
    triangleNormal normals [i1, i2, i3] =
        foldl (flip (uncurry replaceAtIndex)) normals indexedNormals
      where
        indexedNormals :: [(GLuint, Vertex3 GLfloat)]
        indexedNormals = [(i1, n1), (i2, n2), (i3, n3)]

        (TexturedVertex p1 _) = genericIndex vertices i1
        (TexturedVertex p2 _) = genericIndex vertices i2
        (TexturedVertex p3 _) = genericIndex vertices i3

        normal, n1, n2, n3 :: Vertex3 GLfloat
        normal = (p2 - p1) * (p3 - p1)

        n1 = genericIndex normals i1 + normal
        n2 = genericIndex normals i2 + normal
        n3 = genericIndex normals i3 + normal
    triangleNormal _ _ = error "triangleNormal didn't get exact 3 elements!"

    replaceAtIndex :: Integral i => i -> a -> [a] -> [a]
    replaceAtIndex n item ls = a ++ (item:b)
      where
        (a, _ : b) = genericSplitAt n ls

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
    drawIndexedTris 4

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
