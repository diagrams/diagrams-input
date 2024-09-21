{-# LANGUAGE FlexibleContexts, TypeFamilies, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Input
-- Copyright   :  (c) 2015 Tillmann Vogt
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Importing external images into diagrams.
-----------------------------------------------------------------------------

module Diagrams.TwoD.Input
    ( loadImageEmbedded
    , loadImageExternal
    ) where

import           Control.Monad (msum)
import           Codec.Picture
import           Codec.Picture.Types  (dynamicMap)

import           Data.Either (isRight)
import           Data.Semigroup
import           Data.Typeable        (Typeable)

import           Diagrams.Core
import           Diagrams.TwoD.Image
import           Diagrams.TwoD.Size
import           Diagrams.TwoD.Types
import qualified Diagrams.TwoD.Text as TT
import           Diagrams.SVG.ReadSVG (readSVGFile, InputConstraints)
import           Diagrams.SVG.Tree (Place)
import           Filesystem.Path.CurrentOS (decodeString)

-- | Load 2d formats given by a filepath and embed them
loadImageEmbedded :: (InputConstraints b n, Renderable (TT.Text n) b, Read n, n ~ Place) 
                   => FilePath -> IO (Either String (QDiagram b V2 n Any))
loadImageEmbedded path = do
  dImg <- readImage path
  svgImg <- readSVGFile (decodeString path)
  return $ if isRight svgImg then svgImg else fmap (image.rasterImage) dImg
 where
   rasterImage img = DImage (ImageRaster img) (dynamicMap imageWidth img) (dynamicMap imageHeight img) mempty

-- | Load 2d formats given by a filepath and make a reference
loadImageExternal :: (InputConstraints b n, Renderable (DImage n External) b) 
                   => FilePath -> IO (Either String (QDiagram b V2 n Any))
loadImageExternal path = do
  --  svgImg <- readSVGFile (decodeString path)
  dImg <- readImage path
  return $ fmap (image.rasterPath) dImg
 where
   rasterPath img = DImage (ImageRef path) (dynamicMap imageWidth img) (dynamicMap imageHeight img) mempty

