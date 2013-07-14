{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveFunctor #-}
module STL (readSTL, loadSTL,
            STL(..), Facet(..), Point(..)) where

import Control.Applicative hiding (many)
import Control.Monad
import qualified Data.Attoparsec.Char8 as AC
import Data.Attoparsec.Combinator
import Data.Attoparsec.Lazy as AL
import Data.ByteString as B hiding (map)
import qualified Data.ByteString.Lazy as BL hiding (map)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.List as L
import Data.Maybe
import Data.Word (Word8)
import Data.Char

import Prelude hiding (getContents)

default (ByteString)

data STL = STL ByteString [Facet Double] deriving Show
data Facet d = Facet { n,x,y,z :: Point d } deriving (Show, Functor)
data Point d = Point d d d deriving (Show, Functor)

emptyF = Facet emptyP emptyP emptyP emptyP
emptyP = Point 0 0 0 :: Point Double

parseSTL :: Parser STL
parseSTL = do
  name <- string "solid " *> takeWhile1 azAZnum <* newline
  facets <- many1 parseFacet
  string (B.concat ["endsolid ", name]) <* newline
  return $ STL name facets
  where
    azAZnum w = (w >= 97 && w <= 121) -- a-z
             || (w >= 65 && w <= 90) -- A-Z
             || (w >= 48 && w <= 57) -- 0-9
             || (w == 95)

parseFacet :: Parser (Facet Double)
parseFacet = do
  no <- (AC.skipSpace *> string "facet normal " *> parsePoint) <?> "need facet normal"
  _  <- (AC.skipSpace *> string "outer loop") <?> "need outerloop"
  xo <- AC.skipSpace *> string "vertex " *> parsePoint
  yo <- AC.skipSpace *> string "vertex " *> parsePoint
  zo <- AC.skipSpace *> string "vertex " *> parsePoint
  AC.skipSpace <* string "endloop\n"
  AC.skipSpace <* string "endfacet\n"
  return $ Facet {n = no,
                  x = xo,
                  y = yo,
                  z = zo}

parsePoint :: Parser (Point Double)
parsePoint = do
  (n1,n2,n3) <- (,,) <$> AC.double <* AC.skipSpace
                     <*> AC.double <* AC.skipSpace
                     <*> AC.double <* newline
  return $ Point n1 n2 n3

newline :: Parser Word8
newline = word8 10

readSTL file =
  case parse parseSTL file of
    Fail a b y -> error $ BLC.unpack a ++ "\n\n" ++ show b ++ "\n" ++ y
    Done c r -> r

loadSTL fname = liftM readSTL $ BL.readFile fname

main = do
  STL n fs <- loadSTL "big.stl"
  print n
  print $ Prelude.length fs
