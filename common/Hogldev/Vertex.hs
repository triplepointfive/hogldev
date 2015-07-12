module Hogldev.Vertex (
    TexturedVertex(..)
  , TNVertex(..)
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


data TNVertex = TNVertex (Vertex3 GLfloat) (TexCoord2 GLfloat) (Vertex3 GLfloat)

instance Storable TNVertex where
    sizeOf ~(TNVertex v t n) = sizeOf v + sizeOf t + sizeOf n
    alignment ~(TNVertex v _ _) = alignment v
    peek ptr = do
        v <- peek (castPtr ptr)
        t <- peekByteOff (castPtr ptr) (sizeOf v)
        n <- peekByteOff (castPtr ptr) (sizeOf v + sizeOf t)
        return $ TNVertex v t n
    poke ptr (TNVertex v t n) = do
        poke (castPtr ptr) v
        pokeByteOff (castPtr ptr) (sizeOf v) t
        pokeByteOff (castPtr ptr) (sizeOf v + sizeOf t) n

