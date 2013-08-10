{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveFunctor #-}
module STL.BinSTL (readBinSTL) where

import Data.ByteString as B hiding (map)
import Data.ByteString.Char8 as B hiding (map)
import qualified Data.ByteString.Lazy as BL hiding (map)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Word
import Control.Monad
import GHC.Float

import Graphics.Rendering.OpenGL (Vertex3(..), GLfloat)

import STL.STL

default (ByteString)

getVector = do
  a <- getFloat32le >>= return . float2Double
  b <- getFloat32le >>= return . float2Double
  c <- getFloat32le >>= return . float2Double
  return $ Vertex3 a b c

getFacet = do
  normal <- getVector
  x <- getVector
  y <- getVector
  z <- getVector
  attr <- getWord16le
  return $ Facet normal x y z

readBinSTL' = do
  getBytes 80
  count <- getWord32le >>= return . fromIntegral :: Get Integer

  facets <- forM [0..count-1] (\_ -> getFacet)
  return facets

readBinSTL contents fname = do
  STL "noname" facets
    where
      facets = runGet readBinSTL' contents

main :: IO ()
main = do
  contents <- BL.getContents
  print $ runGet readBinSTL' contents
