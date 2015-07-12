module Hogldev.Technique (
    addShader
  , finalize
  , enableTechnique
  , getUniformLocation
) where

import           Control.Monad (unless)
import           System.Exit (exitFailure)

import           Graphics.Rendering.OpenGL

addShader :: Program -> String -> ShaderType -> IO ()
addShader shaderProgram shaderText shaderType = do
    shaderObj <- createShader shaderType
    shaderSourceBS shaderObj $= packUtf8 shaderText

    compileShader shaderObj
    compileStatus shaderObj >>= \ status -> unless status $ do
        errorLog <- shaderInfoLog shaderObj
        putStrLn ("Error compiling shader type " ++ show shaderType
            ++ ": '" ++ errorLog ++ "'")
        exitFailure

    attachShader shaderProgram shaderObj

finalize :: Program -> IO ()
finalize program = do
    linkProgram program
    linkStatus program >>= \ status -> unless status $ do
        errorLog <- programInfoLog program
        putStrLn $ "Error linking shader program: '" ++ errorLog ++ "'"
        exitFailure

    validateProgram program
    validateStatus program >>= \ status -> unless status $ do
        errorLog <- programInfoLog program
        putStrLn $ "Invalid shader program: '" ++ errorLog ++ "'"
        exitFailure

enableTechnique :: Program -> IO ()
enableTechnique shaderProgram = currentProgram $= Just shaderProgram

getUniformLocation :: Program -> String -> IO UniformLocation
getUniformLocation = uniformLocation
