{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
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

module Diagrams.TwoD.Image
    (
      DImage(..), ImageData(..)
    , Embedded, External, Native
    , image
    , loadImageEmb
    , loadImageExt
    , uncheckedImageRef
    , raster
--    , rasterDia
    ) where

import           Control.Monad -- mplus
import           Codec.Picture
import           Codec.Picture.Types  (dynamicMap)

import           Data.Colour          (AlphaColour)
import           Data.Semigroup
import           Data.Typeable        (Typeable)

import           Diagrams.Core

import           Diagrams.Attributes  (colorToSRGBA)
import           Diagrams.TwoD.Path   (isInsideEvenOdd)
import           Diagrams.TwoD.Shapes (rect)
import           Diagrams.TwoD.Types
import           Diagrams.SVG.ReadSVG (readSVGFile, InputConstraints)

import           Linear.Affine
import           Filesystem.Path (FilePath)
import           Filesystem.Path.CurrentOS (encodeString, decodeString)
import           Prelude hiding (FilePath)

data Embedded deriving Typeable
data External deriving Typeable
data Native (t :: *) deriving Typeable
data FP b = FP FilePath

-- | 'ImageData' is either 'Embedded' or a reference tagged as 'External'.
--   The image data is a JuicyPixels @DynamicImage@ or a diagram that contains 
--   vector and raster graphics (e.g. SVG).
--   Additionally 'Native' is provided for external libraries to hook into.
data ImageData t b where
  ImageRaster :: DynamicImage -> ImageData Embedded b
  ImageRef    :: FP b -> ImageData External b -- references also need propagated type class constraints of b
  ImageDiagram :: Diagram b -> ImageData Embedded b
  ImageDiagramRef :: FilePath -> ImageData External b
  ImageNative :: t -> ImageData (Native t) b

-------------------------------------------------------------------------------
-- | An image primitive, the two ints are width followed by height.
--   Will typically be created by @loadImageEmb@ or @loadImageExt@ which,
--   will handle setting the width and height to the actual width and height
--   of the image.
data DImage :: * -> * -> * -> * where
  DImage :: ImageData t b -> Int -> Int -> Transformation V2 n -> DImage b n t
  deriving Typeable

type instance V (DImage b n a) = V2
type instance N (DImage b n a) = n

instance Fractional n => Transformable (DImage b n a) where
  transform t1 (DImage iD w h t2) = DImage iD w h (t1 <> t2)

instance Fractional n => HasOrigin (DImage b n a) where
  moveOriginTo p = translate (origin .-. p)

-- | Make a 'DImage' into a 'Diagram'.
image :: (V b ~ V2, N b ~ n, TypeableFloat n, Typeable a, Typeable b, Renderable (DImage b n a) b)
      => DImage b n a -> QDiagram b V2 n Any

image (DImage (ImageDiagram img) _ _ _) = img
image img
  = mkQD (Prim img)
         (getEnvelope r)
         (getTrace r)
         mempty
         (Query $ \p -> Any (isInsideEvenOdd p r))
  where
    r = rect (fromIntegral w) (fromIntegral h)
    DImage _ w h _ = img


-- | Use JuicyPixels or svg-diagrams to read an image in any format and wrap it in a 'DImage'.
--   The width and height of the image are set to their actual values.
loadImageEmb :: InputConstraints b n => FilePath -> IO (Either String (DImage b n Embedded))
loadImageEmb path = do
  dImg <- readImage (encodeString path)
  svgImg <- readSVGFile path
  return $ msum  [ fmap rasterImage dImg,
                   fmap svgImage svgImg ] -- skip "Left"s and use the first "Right" image
  where
    rasterImage img = DImage (ImageRaster img) (dynamicMap imageWidth img) (dynamicMap imageHeight img) mempty
    svgImage    img = DImage (ImageDiagram img) 1 1 mempty

-- | Check that a file exists, and use JuicyPixels or svg-diagrams to figure out
--   the right size, but save a reference to the image instead
--   of the raster data
loadImageExt :: InputConstraints b n => FP b -> IO (Either String (DImage b n External))
loadImageExt (FP path) = do
  dImg <- readImage (encodeString path)
--  svgImg <- readSVGFile path
  return $ msum [ fmap rasterPath dImg ]
--                  fmap svgPath svgImg ] -- skip "Left"s and use the first "Right" image
  where
    rasterPath img = DImage (ImageRef (FP path)) (dynamicMap imageWidth img) (dynamicMap imageHeight img) mempty
    svgPath    img = DImage (ImageRef (FP path)) 1 1 mempty


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

