{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------
-- |
-- Module    : Diagrams.SVG.ReadSVG
-- Copyright : (c) 2015 Tillmann Vogt <tillk.vogt@googlemail.com>
-- License   : BSD3
--
-- Maintainer: diagrams-discuss@googlegroups.com
-- Stability : stable
-- Portability: portable

-----------------------------------------------------------------------------------------------------

module Diagrams.SVG.ReadSVG
    (
    -- * Main functions
      PreserveAR(..)
    , AlignSVG(..)
    , Place(..)
    , MeetOrSlice(..)
    , InputConstraints(..)
    , readSVGFile
    , preserveAspectRatio
    , nodes
    , insertRefs
    -- * Parsing of basic structure tags
    , parseSVG
    , parseG
    , parseDefs
    , parseSymbol
    , parseUse
    , parseSwitch
    , parseDesc
    , parseTitle
    , parseMetaData
    -- * Parsing of basic shape tags
    , parseRect
    , parseCircle
    , parseEllipse
    , parseLine
    , parsePolyLine
    , parsePolygon
    , parsePath
    -- * Parsing of Gradient tags
    , parseLinearGradient
    , parseRadialGradient
    , parseSet
    , parseStop
    -- * Parsing of other tags
    , parseClipPath
    , parsePattern
    , parseFilter
    , parseImage
    , parseText
    -- * Parsing data uri in <image>
    , image
    , dataUriToImage
    , ImageData(..)
    , DImage(..)
    , Embedded
    , External
    , Native
    , FP(..)
    ) where

import           Codec.Picture
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.ByteString as ABS
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Colour
import qualified Data.Conduit.List as C
import qualified Data.HashMap.Strict as H
import           Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Text as T
import           Data.Text(Text(..))
import           Data.Text.Encoding
import           Data.Typeable (Typeable)
import           Data.XML.Types
import           Diagrams.Attributes
import           Diagrams.Prelude
import           Diagrams.TwoD.Ellipse
import           Diagrams.TwoD.Path   (isInsideEvenOdd)
import           Diagrams.TwoD.Size
import           Diagrams.TwoD.Types
import           Diagrams.SVG.Arguments
import           Diagrams.SVG.Attributes
import           Diagrams.SVG.Path (commands, commandsToTrails, PathCommand(..))
import           Diagrams.SVG.Tree
import           Filesystem.Path (FilePath, extension)
import           Prelude hiding (FilePath)
import           Text.XML.Stream.Parse hiding (parseText)
import           Text.CSS.Parse (parseBlocks)
import           Debug.Trace

-- The following code was included here, because parseImage needs it
-- and there can be no cyclic dependency (ReadSVG.hs importing Image.hs and vice versa)

type instance V (DImage b n a) = V2
type instance N (DImage b n a) = n

instance Fractional n => Transformable (DImage b n a) where
  transform t1 (DImage iD w h t2) = DImage iD w h (t1 <> t2)

instance Fractional n => HasOrigin (DImage b n a) where
  moveOriginTo p = translate (origin .-. p)

data Embedded deriving Typeable
data External deriving Typeable
data Native (t :: *) deriving Typeable
data FP b = FP FilePath

-------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------------
-- | Main library function
--
-- @
-- \{-\# LANGUAGE OverloadedStrings \#-\}
--
-- module Main where
-- import Diagrams.SVG.ReadSVG
-- import Diagrams.Prelude
-- import Diagrams.Backend.SVG.CmdLine
-- import System.Environment
-- import Filesystem.Path.CurrentOS
-- import Diagrams.SVG.Attributes (PreserveAR(..), AlignSVG(..), Place(..), MeetOrSlice(..))
--
-- main = do
--    diagramFromSVG <- readSVGFile \"svgs/web.svg\"
--    mainWith $ diagramFromSVG
-- @
--
readSVGFile :: (V b ~ V2, N b ~ n, RealFloat n, Renderable (Path V2 n) b, Renderable (DImage b n Embedded) b,
                Typeable b, Typeable n, Show n) => FilePath -> IO (Either String (Diagram b))
readSVGFile fp = if (extension fp) /= (Just "svg") then return $ Left "Not a svg file" else
  runResourceT $ runEitherT $ do
    tree <- lift (parseFile def fp $$ force "error in parseSVG" parseSVG)
    right $ diagram tree

diagram :: (RealFloat n, V b ~ V2, n ~ N b, Typeable n, Show n) => Tag b n -> Diagram b
diagram tr = (insertRefs ((nmap,cssmap,gradmap),(0,0,100,100)) tr) # scaleY (-1) # initialStyles
  where
    (ns,css,grad) = nodes Nothing ([],[],[]) tr
    nmap    = H.fromList ns -- needed because of the use-tag and clipPath
    cssmap  = H.fromList css -- CSS inside the <defs> tag
    gradmap = H.fromList $ map textureFromViewBoxCSS grad
    textureFromViewBoxCSS (id1, Gr (Just vb) f) = (id1, f (cssmap,vb))
    textureFromViewBoxCSS (id1, Gr Nothing f)   = (id1, f (cssmap,(0,0,100,100)))

-- Gr (Maybe (ViewBox n)) ((CSSMap,ViewBox n) -> Texture n)

-- | preserveAspectRatio is needed to fit an image into a frame that has a different aspect ratio than the image
--  (e.g. 16:10 against 4:3).
--  SVG embeds images the same way: <http://www.w3.org/TR/SVG11/coords.html#PreserveAspectRatioAttribute>
--
-- > import Graphics.SVGFonts
-- >
-- > portrait preserveAR width height = stroke (readSVGFile preserveAR width height "portrait.svg") # showOrigin
-- > text' t = stroke (textSVG' $ TextOpts t lin INSIDE_H KERN False 1 1 ) # fc back # lc black # fillRule EvenOdd
-- > portraitMeet1 x y = (text' "PAR (AlignXY " ++ show x ++ " " show y ++ ") Meet") ===
-- >                     (portrait (PAR (AlignXY x y) Meet) 200 100 <> rect 200 100)
-- > portraitMeet2 x y = (text' "PAR (AlignXY " ++ show x ++ " " show y ++ ") Meet") ===
-- >                     (portrait (PAR (AlignXY x y) Meet) 100 200 <> rect 100 200)
-- > portraitSlice1 x y = (text' "PAR (AlignXY " ++ show x ++ " " show y ++ ") Slice") ===
-- >                      (portrait (PAR (AlignXY x y) Slice) 100 200 <> rect 100 200)
-- > portraitSlice2 x y = (text' "PAR (AlignXY " ++ show x ++ " " show y ++ ") Slice") ===
-- >                      (portrait (PAR (AlignXY x y) Slice) 200 100 <> rect 200 100)
-- > meetX = (text' "meet") === (portraitMeet1 0 0 ||| portraitMeet1 0.5 0 ||| portraitMeet1 1 0)
-- > meetY = (text' "meet") === (portraitMeet2 0 0 ||| portraitMeet2 0 0.5 ||| portraitMeet2 0 1)
-- > sliceX = (text' "slice") === (portraitSlice1 0 0 ||| portraitSlice1 0.5 0 ||| portraitSlice1 1 0)
-- > sliceY = (text' "slice") === (portraitSlice2 0 0 ||| portraitSlice2 0 0.5 ||| portraitSlice2 0 1)
-- > im = (text' "Image to fit") === (portrait (PAR (AlignXY 0 0) Meet) 123 456)
-- > viewport1 = (text' "Viewport1") === (rect 200 100)
-- > viewport2 = (text' "Viewport2") === (rect 100 200)
-- > imageAndViewports = im === viewport1 === viewport2
-- >
-- > par = imageAndViewports ||| ( ( meetX ||| meetY) === ( sliceX ||| sliceY) )
--
-- <<diagrams/src_Graphics_SVGFonts_ReadFont_textPic0.svg#diagram=par&width=300>>

-- preserveAspectRatio :: Width -> Height -> Width -> Height -> PreserveAR -> Diagram b -> Diagram b
preserveAspectRatio newWidth newHeight oldWidth oldHeight preserveAR image
   | aspectRatio < newAspectRatio = xPlace preserveAR image
   | otherwise                    = yPlace preserveAR image
  where aspectRatio = oldWidth / oldHeight
        newAspectRatio = newWidth / newHeight
        scaX = newHeight / oldHeight
        scaY = newWidth / oldWidth
        xPlace (PAR (AlignXY x y) Meet) i = i # scale scaX # alignBL # translateX ((newWidth  - oldWidth*scaX)*x)
        xPlace (PAR (AlignXY x y) Slice) i = i # scale scaY # alignBL # translateX ((newWidth  - oldWidth*scaX)*x)
--                                               # view (p2 (0, 0)) (r2 (newWidth, newHeight))

        yPlace (PAR (AlignXY x y) Meet) i = i # scale scaY # alignBL # translateY ((newHeight - oldHeight*scaY)*y)
        yPlace (PAR (AlignXY x y) Slice) i = i # scale scaX # alignBL # translateY ((newHeight - oldHeight*scaY)*y)
--                                               # view (p2 (0, 0)) (r2 (newWidth, newHeight))

------------------------------------------------------------------------------------------------------------

-- | Lookup a diagram and return an empty diagram in case the SVG-file has a wrong reference
lookUp hmap i | isJust l  = fromJust l
              | otherwise = Leaf Nothing mempty mempty -- an empty diagram if we can't find the id
  where l = H.lookup i hmap

-- | Evaluate the tree into a diagram by inserting references, applying clipping and passing the viewbox to the leafs
insertRefs :: (V b ~ V2, N b ~ n, RealFloat n, Show n) => (HashMaps b n, ViewBox n) -> Tag b n -> Diagram b
insertRefs (maps,viewbox) (Leaf id1 path f) = f (maps,viewbox)
insertRefs (maps,viewbox) (Grad _ _ _) = mempty
insertRefs (maps,viewbox) (Stop f)   = mempty
insertRefs (maps,viewbox) (Reference selfId id1 (w,h) styles)
    | (isJust w && (fromJust w) <= 0) || (isJust h && (fromJust h) <= 0) = mempty
    | otherwise = referencedDiagram # styles (maps,viewbox)
                                 -- # stretchViewBox (fromJust w) (fromJust h) viewboxPAR
                                    # cutOutViewBox viewboxPAR
  where viewboxPAR = getViewboxPreserveAR subTree
        referencedDiagram = insertRefs (maps,viewbox) (makeSubTreeVisible viewbox subTree)
        subTree = lookUp (sel1 maps) (Diagrams.SVG.Attributes.fragment id1) -- :: Tag
        getViewboxPreserveAR (SubTree _ id1 viewbox ar g children) = (viewbox, ar)
        getViewboxPreserveAR _ = (Nothing, Nothing)
        sel1 (a,b,c) = a

insertRefs (maps,viewbox) (SubTree False _ _ _ _ _) = mempty -- don't display subtrees from the <defs> section
insertRefs (maps,viewbox) (SubTree True id1 viewb ar styles children) =
    subdiagram # styles maps
             --  # stretchViewBox (Diagrams.TwoD.Size.width subdiagram) (Diagrams.TwoD.Size.height subdiagram) (viewbox, ar)
               # cutOutViewBox (viewb, ar)
  where subdiagram = mconcat (map (insertRefs (maps, fromMaybe viewbox viewb)) children)


makeSubTreeVisible viewbox (SubTree _    id1 vb ar g children) =
                           (SubTree True id1 (Just viewbox) ar g (map (makeSubTreeVisible viewbox) children))
makeSubTreeVisible _ x = x

fragment x = fromMaybe T.empty $ fmap snd (parseTempl parseIRI x) -- look only for the text after "#"

stretchViewBox w h ((Just (minX,minY,width,height), Just par)) = preserveAspectRatio w h width height par
stretchViewBox w h ((Just (minX,minY,width,height), Nothing))  =
                                    preserveAspectRatio w h width height (PAR (AlignXY 0.5 0.5) Meet)
stretchViewBox w h _ = id

cutOutViewBox (Just (minX,minY,width,height), _) = rectEnvelope (p2 (minX, minY)) (r2 ((width - minX), (height - minY)))
                                                 --  (clipBy (rect (width - minX) (height - minY)))
cutOutViewBox _ = id

-------------------------------------------------------------------------------------
-- Basic SVG structure

class (V b ~ V2, N b ~ n, RealFloat n, Renderable (Path V2 n) b, Typeable n, Typeable b,
       Renderable (DImage b n Embedded) b, Show n) => InputConstraints b n

instance (V b ~ V2, N b ~ n, RealFloat n, Renderable (Path V2 n) b, Typeable n, Typeable b,
          Renderable (DImage b n Embedded) b, Show n) => InputConstraints b n

-- | Parse \<svg\>, see <http://www.w3.org/TR/SVG/struct.html#SVGElement>
parseSVG :: (MonadThrow m, InputConstraints b n) => Sink Event m (Maybe (Tag b n))
parseSVG = tagName "{http://www.w3.org/2000/svg}svg" svgAttrs $
   \(cpa,ca,gea,pa,class_,style,ext,x,y,w,h,vb,ar,zp,ver,baseprof,cScripT,cStyleT,xmlns,xml) ->
   do gs <- many gContent
      let -- st :: (RealFloat n, RealFloat a, Read a) => HashMaps b n -> [SVGStyle n a]
          st hmaps = (parseStyles style hmaps) ++ -- parse the style attribute (style="stop-color:#000000;stop-opacity:0.8")
                     (parsePA  pa  hmaps) ++ -- presentation attributes: stop-color="#000000" stop-opacity="0.8"
                     (cssStylesFromMap hmaps "svg" (id1 ca) class_)
      return $ -- Debug.Trace.trace ("@" ++ show vb ++ show (parseViewBox vb w h)) (
               SubTree True (id1 ca)
                            (parseViewBox vb w h)
                            (parsePreserveAR ar)
                            (applyStyleSVG st) -- (HashMaps b n -> [SVGStyle n a]) -> a -> a
                            (reverse gs)

svgContent :: (MonadThrow m, InputConstraints b n) => Consumer Event m (Maybe (Tag b n))
svgContent = choose -- the likely most common are checked first
     [parseG, parsePath, parseCircle, parseRect, parseEllipse, parseLine, parsePolyLine, parsePolygon,
      parseDefs, parseSymbol, parseUse, -- structural elements
      parseClipPath, parseText, parsePattern, parseImage, parseSwitch, parseSodipodi,
      parseDesc, parseMetaData, parseTitle] -- descriptive Elements

---------------------------------------------------------------------------
-- | Parse \<g\>, see <http://www.w3.org/TR/SVG/struct.html#GElement>
parseG :: (MonadThrow m, InputConstraints b n) => Consumer Event m (Maybe (Tag b n))
parseG = tagName "{http://www.w3.org/2000/svg}g" gAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr) ->
   do insideGs <- many gContent
      let st hmaps = (parseStyles style hmaps) ++
                     (parsePA  pa  hmaps) ++
                     (cssStylesFromMap hmaps "g" (id1 ca) class_)
      return $ SubTree True (id1 ca)
                            Nothing
                            Nothing
                            ( (applyTr (parseTr tr)) . (applyStyleSVG st) )
                            (reverse insideGs)

gContent :: (MonadThrow m, InputConstraints b n) => Consumer Event m (Maybe (Tag b n))
gContent = choose -- the likely most common are checked first
     [parsePath, parseG, parseRect, parseCircle, parseEllipse, parseLine, parsePolyLine, parsePolygon,
      parseUse, parseSymbol, parseStyle, parseDefs, -- structural elements
      parseText, parseClipPath, parseLinearGradient, parseRadialGradient, parseImage, parseFilter,
      parsePattern, parseSwitch, parsePerspective,
      parseDesc, parseMetaData, parseTitle, parsePathEffect] -- descriptive Elements

---------------------------------------------------------------------------
-- | Parse \<defs\>, see <http://www.w3.org/TR/SVG/struct.html#DefsElement>
parseDefs :: (MonadThrow m, InputConstraints b n) => Consumer Event m (Maybe (Tag b n))
parseDefs = tagName "{http://www.w3.org/2000/svg}defs" gAttrs $
   \(cpa,ca,gea,pa,class_,style,ext,tr) ->
   do insideDefs <- many gContent
      let st hmaps = (parseStyles style hmaps) ++
                     (parsePA pa hmaps) ++
                     (cssStylesFromMap hmaps "defs" (id1 ca) class_)
      return $ SubTree False (id1 ca)
                             Nothing
                             Nothing
                             ( (applyTr (parseTr tr)) . (applyStyleSVG st) )
                             (reverse insideDefs)

---------------------------------------------------------------------------
-- | Parse \<defs\>, see <http://www.w3.org/TR/SVG/struct.html#DefsElement>
-- e.g.
--  <style type="text/css">
--   <![CDATA[
--    .fil0 {fill:#FEFEFE}
--    .fil1 {fill:#3A73B8}
--   ]]>
--  </style>
parseStyle :: (MonadThrow m, RealFloat n) => Consumer Event m (Maybe (Tag b n))
parseStyle = tagName "{http://www.w3.org/2000/svg}style" sAttrs $
   \(ca,type_,media,title) ->
   do insideStyle <- content
      let blocks = parseBlocks insideStyle -- parseBlocks :: Text -> Either String [CssBlock]
      let cssBlocks = case blocks of
                   Left err -> []
                   Right st -> st
      return $ StyleTag cssBlocks -- type CssBlock = (Text, [(Text, Text)]) = (selector, [(attribute, value)])

-----------------------------------------------------------------------------------
-- | Parse \<symbol\>, see <http://www.w3.org/TR/SVG/struct.html#SymbolElement>
parseSymbol :: (MonadThrow m, InputConstraints b n) => Consumer Event m (Maybe (Tag b n))
parseSymbol = tagName "{http://www.w3.org/2000/svg}symbol" symbolAttrs $
   \(ca,gea,pa,class_,style,ext,ar,viewbox) ->
   do insideSym <- many gContent
      let st hmaps = (parseStyles style hmaps) ++
                     (parsePA  pa  hmaps) ++
                     (cssStylesFromMap hmaps "symbol" (id1 ca) class_)
      return $ SubTree False (id1 ca)
                             (parseViewBox viewbox Nothing Nothing)
                             (parsePreserveAR ar)
                             (applyStyleSVG st)
                             (reverse insideSym)

-----------------------------------------------------------------------------------
-- | Parse \<use\>, see <http://www.w3.org/TR/SVG/struct.html#UseElement>
parseUse :: (MonadThrow m, V b ~ V2, N b ~ n, RealFloat n, Typeable n) => Consumer Event m (Maybe (Tag b n))
parseUse = tagName "{http://www.w3.org/2000/svg}use" useAttrs
   $ \(ca,cpa,gea,pa,xlink,class_,style,ext,tr,x,y,w,h) ->
   do -- insideUse <- many useContent
      let st hmaps = (parseStyles style hmaps) ++
                     (parsePA  pa  hmaps) ++
                     (cssStylesFromMap hmaps "use" (id1 ca) class_)
      return $ Reference (id1 ca)
                         (xlinkHref xlink)
                         (parseToDouble w, parseToDouble h) -- TODO use p
                         (f tr x y st) -- f gets supplied with the missing maps an viewbox when evaluating the Tag-tree
  where -- f :: Maybe Text -> Maybe Text -> Maybe Text -> (HashMaps b n -> [SVGStyle n a]) 
        -- -> (HashMaps b n, (n,n,n,n)) -> Diagram b -> Diagram b
        f tr x y st (maps,(minx,miny,vbW,vbH)) = (translate (r2 (p (vbW, minx) 0 x, 
                                                                 p (vbH, miny) 0 y))) .
                                                 (applyTr (parseTr tr)) . (applyStyleSVG st maps)

useContent :: (MonadThrow m, V b ~ V2, N b ~ n, RealFloat n) => Consumer Event m (Maybe (Tag b n))
useContent = choose [parseDesc,parseTitle] -- descriptive elements

--------------------------------------------------------------------------------------
-- | Parse \<switch\>, see <http://www.w3.org/TR/SVG/struct.html#SwitchElement>
parseSwitch :: (MonadThrow m, V b ~ V2, N b ~ n, RealFloat n) => Consumer Event m (Maybe (Tag b n))
parseSwitch = tagName "{http://www.w3.org/2000/svg}switch" switchAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr) ->
   do -- insideSwitch <- many switchContent
      return $ Leaf (id1 ca) mempty mempty

-- switchContent :: (MonadThrow m, V b ~ V2, N b ~ n, RealFloat n) => Consumer Event m (Maybe (Tag b n))
switchContent = choose [parsePath, parseRect, parseCircle, parseEllipse, parseLine, parsePolyLine, parsePolygon]

-----------------------------------------------------------------------------------
-- | Parse \<rect\>,  see <http://www.w3.org/TR/SVG11/shapes.html#RectElement>
parseRect :: (MonadThrow m, InputConstraints b n) => Consumer Event m (Maybe (Tag b n))
parseRect = tagName "{http://www.w3.org/2000/svg}rect" rectAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,ar,tr,x,y,w,h,rx,ry) -> do
    let st hmaps = (parseStyles style hmaps) ++
                   (parsePA  pa  hmaps) ++
                   (cssStylesFromMap hmaps "rect" (id1 ca) class_)
    let rRect pw ph prx pry | prx == 0 && pry == 0 = rect pw ph
                            | otherwise = roundedRect pw ph (if prx == 0 then pry else prx)
    let path (minx,miny,vbW,vbH) = (rRect (p (minx,vbW) 0 w)  (p (miny,vbH) 0 h)
                                          (p (minx,vbW) 0 rx) (p (miny,vbH) 0 ry))
                                   # alignBL
                                   # applyTr (parseTr tr)
                                   # translate (r2 (p (minx,vbW) 0 x, p (miny,vbH) 0 y))
    let f (maps,viewbox) = path viewbox # stroke # applyStyleSVG st maps
    return $ Leaf (id1 ca) path f

---------------------------------------------------------------------------------------------------
-- | Parse \<circle\>,  see <http://www.w3.org/TR/SVG11/shapes.html#CircleElement>
parseCircle :: (MonadThrow m, InputConstraints b n) => Consumer Event m (Maybe (Tag b n))
parseCircle = tagName "{http://www.w3.org/2000/svg}circle" circleAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,r,cx,cy) -> do
    let st :: (RealFloat n, RealFloat a, Read a) => HashMaps b n -> [SVGStyle n a]
        st hmaps = (parseStyles style hmaps) ++
                   (parsePA  pa  hmaps) ++
                   (cssStylesFromMap hmaps "circle" (id1 ca) class_)
    let path (minx,miny,w,h) = circle (p (minx,w) 0 r) -- TODO: radius of a circle in percentages (relative to x?)
                               # applyTr (parseTr tr)
                               # translate (r2 (p (minx,w) 0 cx, p (miny,h) 0 cy))
    let f (maps,viewbox) = path viewbox # stroke # applyStyleSVG st maps
    return $ Leaf (id1 ca) path f

---------------------------------------------------------------------------------------------------
-- | Parse \<ellipse\>,  see <http://www.w3.org/TR/SVG11/shapes.html#EllipseElement>
parseEllipse :: (MonadThrow m, InputConstraints b n) => Consumer Event m (Maybe (Tag b n))
parseEllipse = tagName "{http://www.w3.org/2000/svg}ellipse" ellipseAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,rx,ry,cx,cy) -> do
    let st hmaps = (parseStyles style hmaps) ++
                   (parsePA  pa  hmaps) ++
                   (cssStylesFromMap hmaps "ellipse" (id1 ca) class_)
    let path (minx,miny,w,h) = ((ellipseXY (p (minx,w) 0 rx) (p (miny,h) 0 ry) ))
                               # applyTr (parseTr tr)
                               # translate (r2 (p (minx,w) 0 cx, p (miny,h) 0 cy))
    let f (maps,viewbox) = path viewbox # stroke # applyStyleSVG st maps
    return $ Leaf (id1 ca) path f

---------------------------------------------------------------------------------------------------
-- | Parse \<line\>,  see <http://www.w3.org/TR/SVG11/shapes.html#LineElement>
parseLine :: (MonadThrow m, InputConstraints b n) => Consumer Event m (Maybe (Tag b n))
parseLine = tagName "{http://www.w3.org/2000/svg}line" lineAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,x1,y1,x2,y2) -> do
    let st hmaps = (parseStyles style hmaps) ++
                   (parsePA  pa  hmaps) ++
                   (cssStylesFromMap hmaps "line" (id1 ca) class_)
    let path (minx,miny,w,h) = (fromSegments [ straight (r2 ((p (minx,w) 0 x2) - (p (minx,w) 0 x1), 
                                                             (p (miny,h) 0 y2) - (p (miny,h) 0 y1))) ])
                               # applyTr (parseTr tr)
                               # translate (r2 (p (minx,w) 0 x1, p (miny,h) 0 y1))
    let f (maps,viewbox) = path viewbox # stroke
                                        # applyStyleSVG st maps
    return $ Leaf (id1 ca) path f

---------------------------------------------------------------------------------------------------
-- | Parse \<polyline\>,  see <http://www.w3.org/TR/SVG11/shapes.html#PolylineElement>
parsePolyLine :: (MonadThrow m, InputConstraints b n) => Consumer Event m (Maybe (Tag b n))
parsePolyLine = tagName "{http://www.w3.org/2000/svg}polyline" polygonAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,points) -> do
    let st hmaps = (parseStyles style hmaps) ++
                   (parsePA  pa  hmaps) ++
                   (cssStylesFromMap hmaps "polyline" (id1 ca) class_)
    let ps = parsePoints (fromJust points)
    let path viewbox = fromVertices (map p2 ps) # applyTr (parseTr tr)
                                                # translate (r2 (head ps))
    let f (maps,viewbox) = fromVertices (map p2 ps) # strokeLine
                                                    # applyTr (parseTr tr)
                                                    # translate (r2 (head ps))
                                                    # applyStyleSVG st maps
    return $ Leaf (id1 ca) path f

--------------------------------------------------------------------------------------------------
-- | Parse \<polygon\>,  see <http://www.w3.org/TR/SVG11/shapes.html#PolygonElement>
parsePolygon :: (MonadThrow m, InputConstraints b n) => Consumer Event m (Maybe (Tag b n))
parsePolygon = tagName "{http://www.w3.org/2000/svg}polygon" polygonAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,points) -> do
    let st hmaps = (parseStyles style hmaps) ++
                   (parsePA  pa  hmaps) ++
                   (cssStylesFromMap hmaps "polygon" (id1 ca) class_)
    let ps = parsePoints (fromJust points)
    let path viewbox = fromVertices (map p2 ps) # applyTr (parseTr tr)
                                                # translate (r2 (head ps))
    let f (maps,viewbox) = fromVertices (map p2 ps) # closeLine
                                                    # strokeLoop
                                                    # applyTr (parseTr tr)
                                                    # translate (r2 (head ps))
                                                    # applyStyleSVG st maps
    return $ Leaf (id1 ca) path f

--------------------------------------------------------------------------------------------------
-- | Parse \<path\>,  see <http://www.w3.org/TR/SVG11/paths.html#PathElement>
parsePath :: (MonadThrow m, InputConstraints b n) => Consumer Event m (Maybe (Tag b n))
parsePath = tagName "{http://www.w3.org/2000/svg}path" pathAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,d,pathLength) -> do
    let st hmaps = (parseStyles style hmaps) ++
                   (parsePA  pa  hmaps) ++
                   (cssStylesFromMap hmaps "path" (id1 ca) class_)
    let path viewbox = (mconcat $ commandsToTrails $ commands d) # applyTr (parseTr tr)
    let f (maps,viewbox) = path viewbox # stroke
                                        # applyStyleSVG st maps
    return $ Leaf (id1 ca) path f

-------------------------------------------------------------------------------------------------
-- | Parse \<clipPath\>, see <http://www.w3.org/TR/SVG/masking.html#ClipPathElement>
parseClipPath :: (MonadThrow m, InputConstraints b n) => Consumer Event m (Maybe (Tag b n))
parseClipPath = tagName "{http://www.w3.org/2000/svg}clipPath" clipPathAttrs $
  \(cpa,ca,pa,class_,style,ext,ar,viewbox) -> do
    insideClipPath <- many clipPathContent
    let st hmaps = (parseStyles style hmaps) ++
                   (parsePA  pa  hmaps) ++
                   (cssStylesFromMap hmaps "clipPath" (id1 ca) class_)
    return $ SubTree False (id1 ca)
                     (parseViewBox viewbox Nothing Nothing)
                     (parsePreserveAR ar)
                     (applyStyleSVG st)
                     (reverse insideClipPath)

clipPathContent :: (MonadThrow m, InputConstraints b n) => Consumer Event m (Maybe (Tag b n))
clipPathContent = choose [parseRect, parseCircle, parseEllipse, parseLine, parsePolyLine, parsePath,
                          parsePolygon, parseText, parseUse]

--------------------------------------------------------------------------------------
-- | Parse \<image\>, see <http://www.w3.org/TR/SVG/struct.html#ImageElement>
-- <image width="28" xlink:href="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABwAAAADCAYAAACAjW/aAAAABmJLR0QA/wD/AP+gvaeTAAAAB3RJTUUH2AkMDx4ErQ9V0AAAAClJREFUGJVjYMACGhoa/jMwMPyH0kQDYvQxYpNsaGjAyibCQrL00dSHACypIHXUNrh3AAAAAElFTkSuQmCC" height="3"/>
parseImage :: (MonadThrow m, V b ~ V2, N b ~ n, RealFloat n, Renderable (DImage b (N b) Embedded) b,
              Typeable b, Typeable n) => Consumer Event m (Maybe (Tag b n))
parseImage = tagName "{http://www.w3.org/2000/svg}image" imageAttrs $
  \(ca,cpa,gea,xlink,pa,class_,style,ext,ar,tr,x,y,w,h) ->
  do return $ Leaf (id1 ca) mempty (\(_,(minx,miny,vbW,vbH)) -> (dataUriToImage (xlinkHref xlink) (p (minx,vbW) 0 w) (p (miny,vbH) 0 h))
                                           # alignBL
                                           # applyTr (parseTr tr)
                                           # translate (r2 (p (minx,vbW) 0 x, p (miny,vbH) 0 y)))
-- TODO aspect ratio

data ImageType = JPG | PNG | SVG
--------------------------------------------------------------------------------
-- | Convert base64 encoded data in <image> to a Diagram b with JuicyPixels
--   input: "data:image/png;base64,..."
dataUriToImage :: (Metric (V b), Ord n, RealFloat n, N b ~ n, V2 ~ V b, Renderable (DImage b n Embedded) b,
                  Typeable b, Typeable n) => Maybe Text -> n -> n -> Diagram b
dataUriToImage _           0 h = mempty
dataUriToImage _           w 0 = mempty
dataUriToImage Nothing     w h = mempty
dataUriToImage (Just text) w h = either (const mempty) id $ ABS.parseOnly dataUri (encodeUtf8 text)
  where
    jpg = do { ABS.string "jpg"; return JPG } -- ABS = Data.Attoparsec.ByteString
    png = do { ABS.string "png"; return PNG }
    svg = do { ABS.string "svg"; return SVG }

    dataUri = do
      ABS.string "data:image/"
      imageType <- ABS.choice [jpg, png, svg]
      ABS.string ";base64," -- assuming currently that this is always used
      base64data <- ABS.many1 ABS.anyWord8
      return $ case im imageType (B.pack base64data) of
                 Right img -> image (DImage (ImageRaster img) (round w) (round h) mempty)
                 Left x -> mempty

im :: ImageType -> B.ByteString -> Either String DynamicImage
im imageType base64data = case Base64.decode base64data of
   Left _ -> Left "diagrams-input: Error decoding data uri in <image>-tag"
   Right b64 -> case imageType of
         JPG -> decodeJpeg b64 -- decodeJpeg :: ByteString -> Either String DynamicImage
         PNG -> decodePng b64
         --  SVG -> preserveAspectRatio w h oldWidth oldHeight ar (readSVGBytes base64data) -- something like that
         _ -> Left "diagrams-input: format not supported in <image>-tag"


-- | Parse \<text\>, see <http://www.w3.org/TR/SVG/text.html#TextElement>
-- TODO: implement
parseText :: (MonadThrow m, V b ~ V2, N b ~ n, RealFloat n) => Consumer Event m (Maybe (Tag b n))
parseText = tagName "{http://www.w3.org/2000/svg}text" textAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,la,x,y,dx,dy,rot,textlen) ->
  do t <- orE contentMaybe parseTSpan
     return $ Leaf (id1 ca) mempty mempty

{-<tspan
         sodipodi:role="line"
         id="tspan2173"
         x="1551.4218"
         y="1056.9836" /> -}

parseTSpan = tagName "{http://www.w3.org/2000/svg}tspan" ignoreAttrs $
   \_ -> do c <- content  -- \(role,id_,x,y) ->
            return ""

--------------------------------------------------------------------------------------
-- Gradients

-- > gradient = mkLinearGradient stops ((-0.5) ^& 0) (0.5 ^& 0) GradPad
-- > sq1 = square 1 # fillTexture  gradient

-- | Parse \<linearGradient\>, see <http://www.w3.org/TR/SVG/pservers.html#LinearGradientElement>
-- example: <linearGradient id="SVGID_2_" gradientUnits="userSpaceOnUse" x1="68.2461" y1="197.6797"
--           x2="52.6936" y2="237.5337" gradientTransform="matrix(1 0 0 -1 -22.5352 286.4424)">
parseLinearGradient :: (MonadThrow m, V b ~ V2, N b ~ n, RealFloat n, Show n) => Consumer Event m (Maybe (Tag b n))
parseLinearGradient = tagName "{http://www.w3.org/2000/svg}linearGradient" linearGradAttrs $
  \(cpa,ca,pa,xlink,class_,style,ext,x1,y1,x2,y2,gradientUnits,gradientTransform,spreadMethod) ->
  do gs <- many gradientContent
     let stops = map getTexture $ concat $ map extractStops gs
     -- stops are lists of functions and everyone of these gets passed the same cssmap
     -- and puts them into a Grad constructor
     let f (css,(minx,miny,w,h)) = mkLinearGradient (concat (map ($ css) stops)) -- (minx,miny,w,h) is the viewbox
                                                    ((p (minx,w) 0 x1) ^& (p (miny,h) 0 y1))
                                                    ((p (minx,w) 0 x2) ^& (p (miny,h) 0 y2)) GradPad
     return $ Grad (id1 ca) Nothing f

gradientContent = choose [parseStop, parseMidPointStop] -- parseSet,
   --   parseDesc, parseMetaData, parseTitle] -- descriptive Elements (rarely used here, so tested at the end)

-- | Parse \<radialGradient\>, see <http://www.w3.org/TR/SVG/pservers.html#RadialGradientElement>
parseRadialGradient :: (MonadThrow m, V b ~ V2, N b ~ n, RealFloat n, Show n) => Consumer Event m (Maybe (Tag b n))
parseRadialGradient = tagName "{http://www.w3.org/2000/svg}radialGradient" radialGradAttrs $
  \(cpa,ca,pa,xlink,class_,style,ext,cx,cy,r,fx,fy,gradientUnits,gradientTransform,spreadMethod) ->
  do gs <- many gradientContent
     let stops = map getTexture $ concat $ map extractStops gs
     let f (css,(minx,miny,w,h)) = mkRadialGradient (concat (map ($ css) stops))
                                                    ((p (minx,w) (p (minx,w) 0 cx) fx) ^& -- focal point fx is set to cx if fx does not exist
                                                     (p (miny,h) (p (miny,h) 0 cy) fy))
                                                    0
                                                    ((p (minx,w) 0             cx) ^& 
                                                     (p (miny,h) 0             cy))
                                                     (p (minx,w) 0             r) --TODO radius percentage relative to x or y?
                                                    GradPad
     return $ Grad (id1 ca) Nothing f

extractStops (SubTree b id1 viewBox ar f children) = concat (map extractStops children)
extractStops (Stop stops) = [Stop stops]
extractStops _ = []

getTexture :: (RealFloat n) => Tag b n -> (CSSMap -> [GradientStop n])
getTexture (Stop stops) = stops . (\css -> (H.empty, css, H.empty))

-- | Parse \<set\>, see <http://www.w3.org/TR/SVG/animate.html#SetElement>
parseSet = tagName "{http://www.w3.org/2000/svg}set" setAttrs $
   \(ca,pa,xlink) ->
   do return $ Leaf (id1 ca) mempty mempty -- "set" ignored so far

-- | Parse \<stop\>, see <http://www.w3.org/TR/SVG/pservers.html#StopElement>
--  e.g. <stop  offset="0.4664" style="stop-color:#000000;stop-opacity:0.8"/>
parseStop = tagName "{http://www.w3.org/2000/svg}stop" stopAttrs $
   \(ca,pa,xlink,class_,style,offset) ->
   do let st hmaps = (parseStyles style empty3) ++
                     (parsePA pa empty3) ++
                     (cssStylesFromMap hmaps "stop" (id1 ca) class_)
      return -- $ Debug.Trace.trace (show offset ++ show (p (0,1) offset)) -- (0,1) means that 50% is 0.5
             $ Stop (\hmaps -> mkStops [getStopTriple (p (0,1) 0 offset) (st hmaps)])

parseMidPointStop = tagName "{http://www.w3.org/2000/svg}midPointStop" stopAttrs $
   \(ca,pa,xlink,class_,style,offset) ->
   do let st hmaps = (parseStyles style empty3) ++
                     (parsePA pa empty3) ++
                     (cssStylesFromMap hmaps "midPointStop" (id1 ca) class_)
      return $ Stop (\hmaps -> mkStops [getStopTriple (p (0,1) 0 offset) (st hmaps)])

empty3 = (H.empty,H.empty,H.empty)

-- (An opaque color, a stop fraction, an opacity).
-- mkStops :: [(Colour Double, Double, Double)] -> [GradientStop n]
-- mkStops [(gray, 0, 1), (white, 0.5, 1), (purple, 1, 1)]

getStopTriple offset styles = -- Debug.Trace.trace (show styles ++ "ä" ++ show colors) -- ++ show (col colors, offset, opacity opacities))
                              (col colors, offset, opacity opacities)
  where col ((Fill c):_) = Debug.Trace.trace ("col: " ++ show ((fromAlphaColour c) :: AlphaColour Double) ) $ fromAlphaColour c
        col _ = white
        opacity ((FillOpacity x):_) = x
        opacity _ =  1
        colors = Prelude.filter isFill styles
        opacities = Prelude.filter isOpacity styles

isFill (Fill _) = True
isFill _        = False

isOpacity (FillOpacity _) = True
isOpacity _           = False

-- | A gradient stop contains a color and fraction (usually between 0 and 1)
--data GradientStop d = GradientStop
--     { _stopColor    :: SomeColor
--     , _stopFraction :: d}

----------------------------------------------------------------------------------------
-- descriptive elements
------------------------------------------------------o	----------------------------------
-- | Parse \<desc\>, see <http://www.w3.org/TR/SVG/struct.html#DescriptionAndTitleElements>
-- parseDesc :: (MonadThrow m, Metric (V b), RealFloat (N b)) => Consumer Event m (Maybe (Tag b n))
parseDesc = tagName "{http://www.w3.org/2000/svg}desc" descAttrs
   $ \(ca,class_,style) ->
   do desc <- content
      return $ Leaf (id1 ca) mempty mempty

-- | Parse \<title\>, see <http://www.w3.org/TR/SVG/struct.html#DescriptionAndTitleElements>
parseTitle = tagName "{http://www.w3.org/2000/svg}title" descAttrs
   $ \(ca,class_,style) ->
   do title <- content
      return $ Leaf (id1 ca) mempty mempty

-- | Parse \<meta\>, see <http://www.w3.org/TR/SVG/struct.html#DescriptionAndTitleElements>
--
-- @
-- An example what metadata contains:
--
--  \<metadata
--     id=\"metadata22\"\>
--    \<rdf:RDF\>
--      \<cc:Work
--         rdf:about=\"\"\>
--        \<dc:format\>image\/svg+xml\<\/dc:format\>
--        \<dc:type
--           rdf:resource=\"http:\/\/purl.org\/dc\/dcmitype\/StillImage\" \/\>
--      \</cc:Work\>
--    \</rdf:RDF\>
--  \</metadata\>
-- @
--
-- parseMetaData :: (MonadThrow m, Metric (V b), RealFloat (N b)) => Consumer Event m (Maybe (Tag b n))
parseMetaData = tagName "{http://www.w3.org/2000/svg}metadata" ignoreAttrs
   $ \_ ->
   do -- meta <- many metaContent
      return $ Leaf Nothing mempty mempty

-- metaContent :: (MonadThrow m, Metric (V b), RealFloat (N b)) => Consumer Event m (Maybe (Tag b n))
metaContent = choose [parseRDF] -- extend if needed

-- parseRDF :: (MonadThrow m, Metric (V b), RealFloat (N b)) => Consumer Event m (Maybe (Tag b n))
parseRDF = tagName "{http://www.w3.org/1999/02/22-rdf-syntax-ns#}RDF" ignoreAttrs
          $ \_ ->
          do -- c <- parseWork
             return $ Leaf Nothing mempty mempty

-- parseWork :: (MonadThrow m, Metric (V b), RealFloat (N b)) => Consumer Event m (Maybe (Tag b n))
parseWork = tagName "{http://creativecommons.org/ns#}Work" ignoreAttrs
   $ \_ ->
   do -- c <- many workContent
      return $ Leaf Nothing mempty mempty

{-
workContent = choose [parseFormat, parseType, parseRDFTitle, parseDate, parseCreator,
                      parsePublisher, parseSource, parseLanguage, parseSubject, parseDescription]

parseCreator = tagName "{http://purl.org/dc/elements/1.1/}creator" ignoreAttrs
   $ \_ -> do { c <- parseAgent ; return $ Leaf Nothing mempty mempty }

parseAgent = tagName "{http://creativecommons.org/ns#}Agent" ignoreAttrs
   $ \_ -> do { c <- parseAgentTitle ; return $ Leaf Nothing mempty mempty }

parsePublisher = tagName "{http://purl.org/dc/elements/1.1/}publisher" ignoreAttrs
   $ \_ -> do { c <- parseAgent ; return $ Leaf Nothing mempty mempty }

parseSubject = tagName "{http://purl.org/dc/elements/1.1/}subject" ignoreAttrs
   $ \_ -> do { c <- parseBag ; return $ Leaf Nothing mempty mempty }

-- parseBag :: (MonadThrow m, Metric (V b), Ord (N b), Floating (N b)) => Consumer Event m (Maybe (Tag b n))
parseBag = tagName "{http://www.w3.org/1999/02/22-rdf-syntax-ns#}Bag" ignoreAttrs
   $ \_ -> do { c <- parseList ; return $ Leaf Nothing mempty mempty }
-}

parseFormat = tagName "{http://purl.org/dc/elements/1.1/}format" ignoreAttrs
   $ \_ -> do { c <- content ; return $ Leaf Nothing mempty mempty }

parseType = tagName "{http://purl.org/dc/elements/1.1/}type" ignoreAttrs
   $ \_ -> do { c <- content ; return $ Leaf Nothing mempty mempty }

parseRDFTitle = tagName "{http://purl.org/dc/elements/1.1/}title" ignoreAttrs
   $ \_ -> do { c <- content ; return $ Leaf Nothing mempty mempty }

parseDate = tagName "{http://purl.org/dc/elements/1.1/}date" ignoreAttrs
   $ \_ -> do { c <- content ; return $ Leaf Nothing mempty mempty }

parseAgentTitle = tagName "{http://purl.org/dc/elements/1.1/}title" ignoreAttrs
   $ \_ -> do { c <- content ; return $ Leaf Nothing mempty mempty }

parseSource = tagName "{http://purl.org/dc/elements/1.1/}source" ignoreAttrs
   $ \_ -> do { c <- content ; return $ Leaf Nothing mempty mempty }

parseLanguage = tagName "{http://purl.org/dc/elements/1.1/}language" ignoreAttrs
   $ \_ -> do { c <- content ; return $ Leaf Nothing mempty mempty }

parseList = tagName "{http://www.w3.org/1999/02/22-rdf-syntax-ns#}li" ignoreAttrs
   $ \_ -> do { c <- content ; return $ Leaf Nothing mempty mempty }

parseDescription = tagName "{http://purl.org/dc/elements/1.1/}description" ignoreAttrs
   $ \_ -> do { c <- content ; return $ Leaf Nothing mempty mempty }

------------------------------------
-- inkscape / sodipodi tags
------------------------------------
parseSodipodi = tagName "{http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd}namedview" namedViewAttrs
   $ \(pc,bc,bo,ot,gt,gut,po,ps,ww,wh,id1,sg,zoom,cx,cy,wx,wy,wm,cl) ->
   do -- c <- parseGrid
      return $ Leaf (Just "") mempty mempty

--    <inkscape:grid
--       type="xygrid"
--       id="grid5177" />
parseGrid = tagName "{http://www.inkscape.org/namespaces/inkscape}grid" ignoreAttrs
   $ \_ ->
   do c <- content
      return $ Leaf Nothing mempty mempty

{-   <inkscape:perspective
       sodipodi:type="inkscape:persp3d"
       inkscape:vp_x="0 : 212.5 : 1"
       inkscape:vp_y="0 : 1000 : 0"
       inkscape:vp_z="428.75 : 212.5 : 1"
       inkscape:persp3d-origin="214.375 : 141.66667 : 1"
       id="perspective5175" />
-}

parsePerspective = tagName "{http://www.inkscape.org/namespaces/inkscape}perspective" perspectiveAttrs
   $ \(typ,vp_x,vp_y,vp_z,persp3d_origin,id_) ->
     return $ Leaf (Just "") mempty mempty

parsePathEffect = tagName "{http://www.inkscape.org/namespaces/inkscape}path-effect" ignoreAttrs
   $ \_ -> return $ Leaf Nothing mempty mempty
--------------------------------------------------------------------------------------
-- sceletons

-- | Parse \<pattern\>, see <http://www.w3.org/TR/SVG/pservers.html#PatternElement>
parsePattern :: (MonadThrow m, InputConstraints b n) => Consumer Event m (Maybe (Tag b n))
parsePattern = tagName "{http://www.w3.org/2000/svg}pattern" patternAttrs $
  \(cpa,ca,pa,class_,style,ext,view,ar,x,y,w,h,pUnits,pCUnits,pTrans) ->
  do c <- content -- insidePattern <- many patternContent
     return $ Leaf (Just "") mempty mempty

patternContent :: (MonadThrow m, InputConstraints b n) => Consumer Event m (Maybe (Tag b n))
patternContent = choose [parseImage]

-- | Parse \<filter\>, see <http://www.w3.org/TR/SVG/filters.html#FilterElement>
parseFilter = tagName "{http://www.w3.org/2000/svg}filter" filterAttrs $
  \(ca,pa,xlink,class_,style,ext,x,y,w,h,filterRes,filterUnits,primUnits) ->
  do -- insideFilter <- many filterContent
     return $ Leaf (id1 ca) mempty mempty

filterContent = choose [ parseFeGaussianBlur,
  parseFeBlend,parseFeColorMatrix,parseFeComponentTransfer,parseFeComposite,parseFeConvolveMatrix, -- filter primitive elments
  parseFeDiffuseLighting,parseFeDisplacementMap,parseFeFlood,parseFeImage,
  parseFeMerge,parseFeMorphology,parseFeOffset,parseFeSpecularLighting,parseFeTile,parseFeTurbulence,
  parseDesc,parseTitle]

--------------------------------------------------------------------------------------
-- filter primitives (currently only sceletons)
--------------------------------------------------------------------------------------

-- | Parse \<feBlend\>, see <http://www.w3.org/TR/SVG/filters.html#feBlendElement>
parseFeBlend = tagName "{http://www.w3.org/2000/svg}feBlend" feBlendAttrs $
   \(ca,pa,fpa,class_,style,in1,in2,mode) -> return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feColorMatrix\>, see <http://www.w3.org/TR/SVG/filters.html#feColorMatrixElement>
parseFeColorMatrix = tagName "{http://www.w3.org/2000/svg}feColorMatrix" feColorMatrixAttrs $
   \(ca,pa,fpa,class_,style,in1,type1,values) -> return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feComponentTransfer\>, see <http://www.w3.org/TR/SVG/filters.html#feComponentTransferElement>
parseFeComponentTransfer = tagName "{http://www.w3.org/2000/svg}feComponentTransfer" feComponentTransferAttrs $
   \(ca,pa,fpa,class_,style,in1) -> return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feComposite\>, see <http://www.w3.org/TR/SVG/filters.html#feCompositeElement>
parseFeComposite = tagName "{http://www.w3.org/2000/svg}feComposite" feCompositeAttrs $
   \(ca,pa,fpa,class_,style,in1,in2,operator,k1,k2,k3,k4) -> return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feConvolveMatrix\>, see <http://www.w3.org/TR/SVG/filters.html#feConvolveMatrixElement>
parseFeConvolveMatrix = tagName "{http://www.w3.org/2000/svg}feConvolveMatrix" feConvolveMatrixAttrs $
   \(ca,pa,fpa,class_,style,order,km,d,bias,tx,ty,em,ku,par) -> return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feDiffuseLighting\>, see <http://www.w3.org/TR/SVG/filters.html#feDiffuseLightingElement>
parseFeDiffuseLighting = tagName "{http://www.w3.org/2000/svg}feDiffuseLighting" feDiffuseLightingAttrs $
   \(ca,pa,fpa,class_,style,in1,surfaceScale,diffuseConstant,kuLength) -> return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feDisplacementMap\>, see <http://www.w3.org/TR/SVG/filters.html#feDisplacementMapElement>
parseFeDisplacementMap = tagName "{http://www.w3.org/2000/svg}feDisplacementMap" feDisplacementMapAttrs $
   \(ca,pa,fpa,class_,style,in1,in2,sc,xChan,yChan) -> return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feFlood\>, see <http://www.w3.org/TR/SVG/filters.html#feFloodElement>
parseFeFlood = tagName "{http://www.w3.org/2000/svg}feFlood" feFloodAttrs $
   \(ca,pa,fpa,class_,style) -> return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feGaussianBlur\>, see <http://www.w3.org/TR/SVG/filters.html#feGaussianBlurElement>
parseFeGaussianBlur = tagName "{http://www.w3.org/2000/svg}feGaussianBlur" feGaussianBlurAttrs $
   \(ca,pa,fpa,class_,style,in1,stdDeviation) -> return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feImage\>, see <http://www.w3.org/TR/SVG/filters.html#feImageElement>
parseFeImage = tagName "{http://www.w3.org/2000/svg}feImage" feImageAttrs $
   \(ca,pa,fpa,xlibk,class_,style,ext,par) -> return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feMerge\>, see <http://www.w3.org/TR/SVG/filters.html#feMergeElement>
parseFeMerge = tagName "{http://www.w3.org/2000/svg}feMerge" feMergeAttrs $
   \(ca,pa,fpa,class_,style) -> return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feMorphology\>, see <http://www.w3.org/TR/SVG/filters.html#feMorphologyElement>
parseFeMorphology = tagName "{http://www.w3.org/2000/svg}feMorphology" feMorphologyAttrs $
   \(ca,pa,fpa,class_,style,in1,operator,radius) -> return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feOffset\>, see <http://www.w3.org/TR/SVG/filters.html#feOffsetElement>
parseFeOffset = tagName "{http://www.w3.org/2000/svg}feOffset" feOffsetAttrs $
   \(ca,pa,fpa,class_,style,in1,dx,dy) -> return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feSpecularLighting\>, see <http://www.w3.org/TR/SVG/filters.html#feSpecularLightingElement>
parseFeSpecularLighting = tagName "{http://www.w3.org/2000/svg}feSpecularLighting" feSpecularLightingAttrs $
   \(ca,pa,fpa,class_,style,in1,surfaceScale,sc,se,ku) -> return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feTile\>, see <http://www.w3.org/TR/SVG/filters.html#feTileElement>
parseFeTile = tagName "{http://www.w3.org/2000/svg}feTile" feTileAttrs $
   \(ca,pa,fpa,class_,style,in1) -> return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feTurbulence\>, see <http://www.w3.org/TR/SVG/filters.html#feTurbulenceElement>
parseFeTurbulence = tagName "{http://www.w3.org/2000/svg}feTurbulence" feTurbulenceAttrs $
   \(ca,pa,fpa,class_,style,in1,in2,mode) -> return $ Leaf (id1 ca) mempty mempty

------------------------------------------------------------------------------------

animationElements = []
