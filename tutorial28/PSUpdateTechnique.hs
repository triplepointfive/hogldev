{-# LANGUAGE RecordWildCards #-}
module PSUpdateTechnique (
    PSUpdateTechnique (..)
  , initPSUpdateTechnique
  , setDeltaTimeMillis
  , setTime
  , setRandomTextureUnit
  , setLauncherLifetime
  , setShellLifetime
  , setSecondaryShellLifetime
  , enablePSUpdateTechnique
) where

import           Graphics.GLUtil
import           Graphics.Rendering.OpenGL

import           Hogldev.Technique

data PSUpdateTechnique =
    PSUpdateTechnique
    { lProgram                   :: !Program
    , lDeltaTimeMillisLoc        :: !UniformLocation
    , lRandomTextureLoc          :: !UniformLocation
    , lTimeLoc                   :: !UniformLocation
    , lLauncherLifetimeLoc       :: !UniformLocation
    , lShellLifetimeLoc          :: !UniformLocation
    , lSecondaryShellLifetimeLoc :: !UniformLocation
    } deriving Show

initPSUpdateTechnique :: IO PSUpdateTechnique
initPSUpdateTechnique = do
    program <- createProgram
    addShader program "tutorial28/billboard.vs" VertexShader
    addShader program "tutorial28/billboard.gs" GeometryShader
    addShader program "tutorial28/billboard.fs" FragmentShader
    -- finalize program

    let varyings = ["Type1", "Position1", "Velocity1", "Age1"]
    setTransformFeedbackVaryings program varyings InterleavedAttribs

    finalize program

    deltaTimeMillisLoc        <- getUniformLocation program "gDeltaTimeMillis"
    randomTextureLoc          <- getUniformLocation program "gRandomTexture"
    timeLoc                   <- getUniformLocation program "gTime"
    launcherLifetimeLoc       <- getUniformLocation program "gLauncherLifetime"
    shellLifetimeLoc          <- getUniformLocation program "gShellLifetime"
    secondaryShellLifetimeLoc <- getUniformLocation program "gSecondaryShellLifetime"

    return PSUpdateTechnique
        { lProgram                   = program
        , lDeltaTimeMillisLoc        = deltaTimeMillisLoc
        , lRandomTextureLoc          = randomTextureLoc
        , lTimeLoc                   = timeLoc
        , lLauncherLifetimeLoc       = launcherLifetimeLoc
        , lShellLifetimeLoc          = shellLifetimeLoc
        , lSecondaryShellLifetimeLoc = secondaryShellLifetimeLoc
        }

setDeltaTimeMillis :: PSUpdateTechnique -> GLfloat -> IO ()
setDeltaTimeMillis PSUpdateTechnique{..} deltaTimeMillis =
    uniformScalar lShellLifetimeLoc $= deltaTimeMillis

setTime :: PSUpdateTechnique -> GLfloat -> IO ()
setTime PSUpdateTechnique{..} time =
    uniformScalar lTimeLoc $= time

setRandomTextureUnit :: PSUpdateTechnique -> GLint -> IO ()
setRandomTextureUnit PSUpdateTechnique{..} textureUnit =
    uniformScalar lRandomTextureLoc $= textureUnit

setLauncherLifetime :: PSUpdateTechnique -> GLfloat -> IO ()
setLauncherLifetime PSUpdateTechnique{..} lifetime =
    uniformScalar lLauncherLifetimeLoc $= lifetime

setShellLifetime :: PSUpdateTechnique -> GLfloat -> IO ()
setShellLifetime PSUpdateTechnique{..} lifetime =
    uniformScalar lShellLifetimeLoc $= lifetime

setSecondaryShellLifetime :: PSUpdateTechnique -> GLfloat -> IO ()
setSecondaryShellLifetime PSUpdateTechnique{..} lifetime =
    uniformScalar lSecondaryShellLifetimeLoc $= lifetime

enablePSUpdateTechnique :: PSUpdateTechnique -> IO ()
enablePSUpdateTechnique PSUpdateTechnique{..} = enableTechnique lProgram
