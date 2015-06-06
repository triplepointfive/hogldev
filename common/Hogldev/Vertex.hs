module Hogldev.Vertex (
    TexturedVertex(..)
) where

import           Foreign.Storable
import           Foreign.Ptr

import           Graphics.Rendering.OpenGL

data TexturedVertex = TexturedVertex (Vertex3 GLfloat) (TexCoord2 GLfloat)

instance Storable TexturedVertex where
    sizeOf ~(TexturedVertex v t) = sizeOf v + sizeOf t
    alignment ~(TexturedVertex v _) = alignment v
    peek ptr = do
        v <- peek (castPtr ptr)
        t <- peekByteOff (castPtr ptr) (sizeOf v)
        return $ TexturedVertex v t
    poke ptr (TexturedVertex v t) = do
        poke (castPtr ptr) v
        pokeByteOff (castPtr ptr) (sizeOf v) t
