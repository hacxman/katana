{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveFunctor #-}
module STL.STL (STL(..), Facet(..), Point(..)) where

import Data.ByteString as B hiding (map)

import Graphics.Rendering.OpenGL (Vertex3(..), GLfloat)

default (ByteString)

data STL = STL ByteString [Facet Double] deriving Show
data Facet d = Facet { n,x,y,z :: Point d } deriving (Show, Functor)
type Point d = Vertex3 d -- Point d d d deriving (Show, Functor)

emptyF = Facet emptyP emptyP emptyP emptyP
emptyP = Vertex3 0 0 0 :: Vertex3 GLfloat
