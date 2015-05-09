{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ConstraintKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Image
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Importing external images into diagrams.
-- Usage: To create a diagram from an embedded image with width 1 and height
--   set according to the aspect ratio: 'image img # scaleUToX 1`
--   where 'img' is a 'DImage v Embedded'
-----------------------------------------------------------------------------

module Diagrams.TwoD.Input
    (
      DImage(..), ImageData(..)
    , Embedded, External, Native
    , image
    , loadImageEmb
    , loadImageExt
    , uncheckedImageRef
--    , raster
--    , rasterDia
    -- * Parsing data uri in <image>
    , dataUriToImage
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
import           Filesystem.Path (FilePath)
import           Filesystem.Path.CurrentOS (encodeString, decodeString)
import           Prelude hiding (FilePath)

-- | Use JuicyPixels or svg-diagrams to read an image in any format and wrap it in a 'DImage'.
--   The width and height of the image are set to their actual values.
loadImageEmb :: InputConstraints b n => FilePath -> IO (Either String (DImage b n Embedded))
loadImageEmb path = do
  dImg <- readImage (encodeString path)
  svgImg <- readSVGFile path
  return $ msum  [ fmap svgImage svgImg,
                   fmap rasterImage dImg ] -- skip "Left"s and use the first "Right" image
  where
    rasterImage img = DImage (ImageRaster img) (dynamicMap imageWidth img) (dynamicMap imageHeight img) mempty
    svgImage    img = DImage (ImageDiagram img) (round $ Diagrams.TwoD.Size.width img) 
                                                (round $ Diagrams.TwoD.Size.height img) mempty

-- | Check that a file exists, and use JuicyPixels or svg-diagrams to figure out
--   the right size, but save a reference to the image instead
--   of the raster data
loadImageExt :: InputConstraints b n => FP b -> IO (Either String (DImage b n External))
loadImageExt (FP path) = do
  dImg <- readImage path
--  svgImg <- readSVGFile path
  return $ msum [ fmap rasterPath dImg ]
--                  fmap svgPath svgImg ] -- skip "Left"s and use the first "Right" image
  where
    rasterPath img = DImage (ImageRef (FP path)) (dynamicMap imageWidth img) (dynamicMap imageHeight img) mempty
--    svgPath    img = DImage (ImageRef (FP path)) (round $ Diagrams.TwoD.Size.width img)
--                                                 (round $ Diagrams.TwoD.Size.height img) mempty

{-
-- | Make an "unchecked" image reference; have to specify a
--   width and height. Unless the aspect ratio of the external
--   image is the w :: h, then the image will be distorted.
uncheckedImageRef :: Num n => FilePath -> Int -> Int -> DImage b n External
uncheckedImageRef path w h = DImage (ImageRef (FP path)) w h mempty

-- | Crate a diagram from raw raster data.
-- rasterDia :: (TypeableFloat n, Renderable (DImage b n Embedded) b)
--           => (Int -> Int -> AlphaColour Double) -> Int -> Int -> QDiagram b V2 n Any
-- rasterDia f w h = image $ raster f w h

-- | Create an image "from scratch" by specifying the pixel data
raster :: Num n => (Int -> Int -> AlphaColour Double) -> Int -> Int -> DImage b n Embedded
raster f w h = DImage (ImageRaster (ImageRGBA8 img)) w h mempty
  where
    img = generateImage g w h
    g x y = fromAlphaColour $ f x y

fromAlphaColour :: AlphaColour Double -> PixelRGBA8
fromAlphaColour c = PixelRGBA8 r g b a
  where
    (r, g, b, a) = (int r', int g', int b', int a')
    (r', g', b', a') = colorToSRGBA c
    int x = round (255 * x)

instance Fractional n => (Renderable (DImage b n a) NullBackend) where
  render _ _ = mempty
-}
