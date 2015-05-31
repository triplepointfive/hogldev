module Main where

import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLUT

main :: IO ()
main = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBAMode]
    initialWindowSize $= Size 1024 768
    initialWindowPosition $= Position 100 100
    createWindow "Tutorial 01"

    initializeGlutCallbacks
    clearColor $= Color4 0 0 0 0

    mainLoop

initializeGlutCallbacks :: IO ()
initializeGlutCallbacks =
    displayCallback $= renderSceneCB

renderSceneCB :: IO ()
renderSceneCB = do
    clear [ColorBuffer]
    swapBuffers
