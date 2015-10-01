module ShadowMapFBO where

import Graphics.Rendering.OpenGL

data ShadowMapFBO

initializeShadowMapFBO :: IO ShadowMapFBO
initializeShadowMapFBO = do
    -- Create the FBO.
    fbo <- genObjectName :: FramebufferObject
    shadowMap <- genObjectName :: TextureObject


