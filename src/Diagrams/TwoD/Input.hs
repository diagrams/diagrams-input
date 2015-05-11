{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ConstraintKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Input
-- Copyright   :  (c) 2015 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Importing external images into diagrams.
-----------------------------------------------------------------------------

module Diagrams.TwoD.Input
    ( loadImageEmbedded
    , loadImageExternal
    ) where

import           Control.Monad -- mplus
import           Codec.Picture
import           Codec.Picture.Types  (dynamicMap)

import           Data.Colour          (AlphaColour)
import           Data.Semigroup
import           Data.Typeable        (Typeable)

import           Diagrams.Core

import           Diagrams.Attributes  (colorToSRGBA)
import           Diagrams.TwoD.Image
import           Diagrams.TwoD.Shapes (rect)
import           Diagrams.TwoD.Size
import           Diagrams.TwoD.Types
import           Diagrams.SVG.ReadSVG (readSVGFile, InputConstraints, dataUriToImage)
import           Linear.Affine
-- import           Filesystem.Path (FilePath)
import           Filesystem.Path.CurrentOS (encodeString, decodeString)
-- import           Prelude hiding (FilePath)

-- | Load 2d formats given by a filepath and embed them
loadImageEmbedded :: InputConstraints b n => FilePath -> IO (Either String (QDiagram b V2 n Any))
loadImageEmbedded path = do
  dImg <- readImage path -- FilePath -> IO (Either String DynamicImage)
  svgImg <- readSVGFile (decodeString path) -- FilePath -> IO (Either String (Diagram b))
  return $ msum  [ svgImg,
                   fmap (image.rasterImage) dImg ] -- skip "Left"s and use the first "Right" image
  where
    rasterImage img = DImage (ImageRaster img) (dynamicMap imageWidth img) (dynamicMap imageHeight img) mempty

-- | Load 2d formats given by a filepath and make a reference if the format allows it
loadImageExternal :: (InputConstraints b n, Renderable (DImage n External) b) => FilePath -> IO (Either String (QDiagram b V2 n Any))
loadImageExternal path = do
  dImg <- readImage path
--  svgImg <- readSVGFile path
  return $ msum [ fmap (image.rasterPath) dImg ]
--                svgImg ] -- skip "Left"s and use the first "Right" image
  where
    rasterPath img = DImage (ImageRef path) (dynamicMap imageWidth img) (dynamicMap imageHeight img) mempty

