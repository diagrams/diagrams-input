{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GADTs,
             MultiParamTypeClasses, NoMonomorphismRestriction, OverloadedStrings, TypeFamilies, UndecidableInstances #-}

-------------------------------------------------------------------
-- |
-- Module     : Diagrams.SVG.ReadSVG
-- Copyright  : (c) 2015 Tillmann Vogt <tillk.vogt@googlemail.com>
-- License    :  BSD-style (see LICENSE)
-- Maintainer :  diagrams-discuss@googlegroups.com
--
-- Maintainer : diagrams-discuss@googlegroups.com
-- Stability  : stable
-- Portability: portable

-------------------------------------------------------------------

module Diagrams.SVG.ReadSVG
    (
    -- * Main functions
      readSVGFile
    , preserveAspectRatio
    , nodes
    , insertRefs
    , PreserveAR(..)
    , AlignSVG(..)
    , Place(..)
    , MeetOrSlice(..)
    , InputConstraints(..)
    -- * Parsing of basic structure tags
    , parseSVG
    , parseG
    , parseDefs
    , parseSymbol
    , parseUse
    , parseSwitch
    , parseDesc
    , parseTitle
--    , parseMetaData
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
    , dataUriToImage
    ) where

import           Codec.Picture
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Class
import           Data.Either.Combinators
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
import           Diagrams.TwoD.Path (isInsideEvenOdd)
import           Diagrams.TwoD.Size
import           Diagrams.TwoD.Types
import qualified Diagrams.TwoD.Text as TT
import           Diagrams.SVG.Arguments
import           Diagrams.SVG.Attributes
import           Diagrams.SVG.Fonts.ReadFont
import           Diagrams.SVG.Path (commands, commandsToPaths, PathCommand(..))
import           Diagrams.SVG.Tree
import           Filesystem.Path (FilePath(..), extension)
import           Filesystem.Path.CurrentOS (encodeString)
import           Prelude hiding (FilePath)
import           Text.XML.Stream.Parse hiding (parseText)
import           Text.CSS.Parse (parseBlocks)

import           Debug.Trace
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
readSVGFile :: (V b ~ V2, N b ~ n, RealFloat n, Renderable (Path V2 n) b, Renderable (DImage n Embedded) b, -- TODO upper example
                Typeable b, Typeable n, Show n, Read n, n ~ Place, Renderable (TT.Text n) b) 
             => Filesystem.Path.FilePath -> IO (Either String (Diagram b))
readSVGFile fp = if (extension fp) /= (Just "svg") then return $ Left "Not a svg file" else -- TODO All exceptions into left values
  runResourceT $ do tree <- parseFile def (encodeString fp) $$ force "error in parseSVG" parseSVG
                    return (Right (diagram tree))

diagram :: (RealFloat n, V b ~ V2, n ~ N b, Typeable n, Read n, n ~ Place) => Tag b n -> Diagram b
diagram tr = (insertRefs ((nmap,cssmap,expandedGradMap),(0,0,100,100)) tr) # scaleY (-1) # initialStyles
  where
    (ns,css,grad,fonts) = nodes Nothing ([],[],[], []) tr
    nmap    = H.fromList ns -- needed because of the use-tag and clipPath
    cssmap  = H.fromList css -- CSS inside the <defs> tag
    gradmap = H.fromList grad
    expandedGradMap = expandGradMap gradmap


-- | Read font data from font file, and compute its outline map.
--
{-
loadFont :: (Read n, RealFloat n) => FilePath -> IO (Either String (PreparedFont n))
loadFont filename = if (extension fp) /= (Just "svg") then return $ Left "Not a svg file" else -- TODO All exceptions into left values
  runResourceT $ runEitherT $ do
    tree <- lift (parseFile def fp $$ force "error in parseSVG" parseSVG)
    let fontData = font tree
    case fontData of Left s -> return (Left s)
                     Right s -> do let (font, errs) = prepareFont fontData
                                   sequence_ [ putStrLn ("error parsing character '" ++ ch ++ "': " ++ err)
                                             | (ch, err) <- Map.toList errs
                                             ]
                                   return font

font tr = fonts
  where (ns,css,grad,fonts) = nodes Nothing ([],[],[], []) tr
-}
-------------------------------------------------------------------------------------
-- Basic SVG structure

tagName name = tag' (Text.XML.Stream.Parse.matching (== name))

class (V b ~ V2, N b ~ n, RealFloat n, Renderable (Path V2 n) b, Typeable n, Typeable b, Show n,
       Renderable (DImage n Embedded) b) => InputConstraints b n

instance (V b ~ V2, N b ~ n, RealFloat n, Renderable (Path V2 n) b, Typeable n, Typeable b, Show n,
          Renderable (DImage n Embedded) b) => InputConstraints b n

-- | Parse \<svg\>, see <http://www.w3.org/TR/SVG/struct.html#SVGElement>
parseSVG :: (MonadThrow m, InputConstraints b n, Renderable (TT.Text n) b, Read n) 
          => Sink Event m (Maybe (Tag b n))
parseSVG = tagName "{http://www.w3.org/2000/svg}svg" svgAttrs $
   \(cpa,ca,gea,pa,class_,style,ext,x,y,w,h,vb,ar,zp,ver,baseprof,cScripT,cStyleT,xmlns,xml) ->
   do gs <- many gContent
      let st hmaps = (parseStyles style hmaps) ++ -- parse the style attribute (style="stop-color:#000000;stop-opacity:0.8")
                     (parsePA  pa  hmaps) ++ -- presentation attributes: stop-color="#000000" stop-opacity="0.8"
                     (cssStylesFromMap hmaps "svg" (id1 ca) class_)
      let pw = if (isJust w) then parseDouble $ fromJust w else 0
      let ph = if (isJust h) then parseDouble $ fromJust h else 0
      return $ -- Debug.Trace.trace ("@" ++ show vb ++ show (parseViewBox vb w h)) (
               SubTree True (id1 ca)
                            (pw,ph)
                            (parseViewBox vb w h)
                            (parsePreserveAR ar)
                            (applyStyleSVG st)
                            (reverse gs)

svgContent :: (MonadThrow m, InputConstraints b n, Renderable (TT.Text n) b, Read n) 
            => Consumer Event m (Maybe (Tag b n))
svgContent = choose -- the likely most common are checked first
     [parseG, parsePath, parseCircle, parseRect, parseEllipse, parseLine, parsePolyLine, parsePolygon,
      parseDefs, parseSymbol, parseUse, -- structural elements
      parseClipPath, parsePattern, parseImage, parseText, -- parseSwitch, parseSodipodi,
      skipArbitraryTag] -- should always be last!
      -- parseDesc, parseMetaData, parseTitle] -- descriptive Elements

---------------------------------------------------------------------------
-- | Parse \<g\>, see <http://www.w3.org/TR/SVG/struct.html#GElement>
parseG :: (MonadThrow m, InputConstraints b n, Renderable (TT.Text n) b, Read n) 
        => Consumer Event m (Maybe (Tag b n))
parseG = tagName "{http://www.w3.org/2000/svg}g" gAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr) ->
   do insideGs <- many gContent
      let st hmaps = (parseStyles style hmaps) ++
                     (parsePA  pa  hmaps) ++
                     (cssStylesFromMap hmaps "g" (id1 ca) class_)
      return $ SubTree True (id1 ca)
                            (0, 0)
                            Nothing
                            Nothing
                            (\maps -> (applyStyleSVG st maps) . (applyTr (parseTr tr)) )
                            (reverse insideGs)

gContent :: (MonadThrow m, InputConstraints b n, Show n, Read n, Renderable (TT.Text n) b) 
          => Consumer Event m (Maybe (Tag b n))
gContent = choose -- the likely most common are checked first
     [parsePath, parseG, parseRect, parseCircle, parseEllipse, parseLine, parsePolyLine, parsePolygon,
      parseUse, parseSymbol, parseStyle, parseDefs, -- structural elements
      parseClipPath, parseLinearGradient, parseRadialGradient, parseImage, parseText, -- parseFont,
      skipArbitraryTag] -- -- should always be last!
--      parseFilter, parsePattern, parseSwitch, parsePerspective,
--      parseDesc, parseMetaData, parseTitle, parsePathEffect] -- descriptive Elements

---------------------------------------------------------------------------
-- | Parse \<defs\>, see <http://www.w3.org/TR/SVG/struct.html#DefsElement>
parseDefs :: (MonadThrow m, InputConstraints b n, Renderable (TT.Text n) b, Read n) 
           => Consumer Event m (Maybe (Tag b n))
parseDefs = tagName "{http://www.w3.org/2000/svg}defs" gAttrs $
   \(cpa,ca,gea,pa,class_,style,ext,tr) ->
   do insideDefs <- many gContent
      let st hmaps = (parseStyles style hmaps) ++
                     (parsePA pa hmaps) ++
                     (cssStylesFromMap hmaps "defs" (id1 ca) class_)
      return $ SubTree False (id1 ca)
                             (0, 0)
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
parseSymbol :: (MonadThrow m, InputConstraints b n, Renderable (TT.Text n) b, Read n) 
             => Consumer Event m (Maybe (Tag b n))
parseSymbol = tagName "{http://www.w3.org/2000/svg}symbol" symbolAttrs $
   \(ca,gea,pa,class_,style,ext,ar,viewbox) ->
   do insideSym <- many gContent
      let st hmaps = (parseStyles style hmaps) ++
                     (parsePA  pa  hmaps) ++
                     (cssStylesFromMap hmaps "symbol" (id1 ca) class_)
      return $ SubTree False (id1 ca)
                             (0, 0)
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
      let path (minx,miny,vbW,vbH) = rect (p (minx,vbW) 0 w)  (p (miny,vbH) 0 h)
      return $ Reference (id1 ca)
                         (Diagrams.SVG.Attributes.fragment $ xlinkHref xlink)
                         path
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
    let -- st :: (RealFloat n, RealFloat a, Read a) => (HashMaps b n, ViewBox n) -> [SVGStyle n a]
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
    let path viewbox = fromVertices (map p2 ps) # translate (r2 (head ps))
                                                # applyTr (parseTr tr)

    let f (maps,viewbox) = fromVertices (map p2 ps) # strokeLine
                                                    # translate (r2 (head ps))
                                                    # applyTr (parseTr tr)
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
    let path viewbox = fromVertices (map p2 ps) # translate (r2 (head ps))
                                                # applyTr (parseTr tr)
    let f (maps,viewbox) = fromVertices (map p2 ps) # closeLine
                                                    # strokeLoop
                                                    # translate (r2 (head ps))
                                                    # applyTr (parseTr tr)
                                                    # applyStyleSVG st maps
    return $ Leaf (id1 ca) path f

--------------------------------------------------------------------------------------------------
-- | Parse \<path\>,  see <http://www.w3.org/TR/SVG11/paths.html#PathElement>
parsePath :: (MonadThrow m, InputConstraints b n, Show n) => Consumer Event m (Maybe (Tag b n))
parsePath = tagName "{http://www.w3.org/2000/svg}path" pathAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,d,pathLength) -> do
    let st hmaps = (parseStyles style hmaps) ++
                   (parsePA  pa  hmaps) ++
                   (cssStylesFromMap hmaps "path" (id1 ca) class_)
    let path viewbox = (mconcat $ commandsToPaths $ commands d) # applyTr (parseTr tr)
    let f (maps,viewbox) = path viewbox # strokePath
                                        # applyStyleSVG st maps
    return $ Leaf (id1 ca) path f

-------------------------------------------------------------------------------------------------
-- | Parse \<clipPath\>, see <http://www.w3.org/TR/SVG/masking.html#ClipPathElement>
parseClipPath :: (MonadThrow m, InputConstraints b n, Show n, Read n, Renderable (TT.Text n) b) 
               => Consumer Event m (Maybe (Tag b n))
parseClipPath = tagName "{http://www.w3.org/2000/svg}clipPath" clipPathAttrs $
  \(cpa,ca,pa,class_,style,ext,ar,viewbox) -> do
    insideClipPath <- many clipPathContent
    let st hmaps = (parseStyles style hmaps) ++
                   (parsePA  pa  hmaps) ++
                   (cssStylesFromMap hmaps "clipPath" (id1 ca) class_)
    return $ SubTree False (id1 ca)
                     (0, 0)
                     (parseViewBox viewbox Nothing Nothing)
                     (parsePreserveAR ar)
                     (applyStyleSVG st)
                     (reverse insideClipPath)

clipPathContent :: (MonadThrow m, InputConstraints b n, Show n, Read n, Renderable (TT.Text n) b) 
                 => Consumer Event m (Maybe (Tag b n))
clipPathContent = choose [parseRect, parseCircle, parseEllipse, parseLine, parsePolyLine, parsePath,
                          parsePolygon, parseText, parseUse]

--------------------------------------------------------------------------------------
-- | Parse \<image\>, see <http://www.w3.org/TR/SVG/struct.html#ImageElement>
-- <image width="28" xlink:href="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABwAAAADCAYAAACAjW/aAAAABmJLR0QA/wD/AP+gvaeTAAAAB3RJTUUH2AkMDx4ErQ9V0AAAAClJREFUGJVjYMACGhoa/jMwMPyH0kQDYvQxYpNsaGjAyibCQrL00dSHACypIHXUNrh3AAAAAElFTkSuQmCC" height="3"/>
parseImage :: (MonadThrow m, V b ~ V2, N b ~ n, RealFloat n, Renderable (DImage (N b) Embedded) b,
              Typeable b, Typeable n) => Consumer Event m (Maybe (Tag b n))
parseImage = tagName "{http://www.w3.org/2000/svg}image" imageAttrs $
  \(ca,cpa,gea,xlink,pa,class_,style,ext,ar,tr,x,y,w,h) ->
  do return $ Leaf (id1 ca) mempty (\(_,(minx,miny,vbW,vbH)) -> (dataUriToImage (xlinkHref xlink) (p (minx,vbW) 0 w) (p (miny,vbH) 0 h))
                                           # alignBL
                                           # applyTr (parseTr tr)
                                           # translate (r2 (p (minx,vbW) 0 x, p (miny,vbH) 0 y)))
-- TODO aspect ratio

data ImageType = JPG | PNG | SVG

---------------------------------------------------------------------------------------------------
-- | Convert base64 encoded data in <image> to a Diagram b with JuicyPixels
--   input: "data:image/png;base64,..."
dataUriToImage :: (Metric (V b), Ord n, RealFloat n, N b ~ n, V2 ~ V b, Renderable (DImage n Embedded) b,
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

-------------------------------------------------------------------------------------------------
-- | Parse \<text\>, see <http://www.w3.org/TR/SVG/text.html#TextElement>
parseText :: (MonadThrow m, InputConstraints b n, Read n, RealFloat n, Renderable (TT.Text n) b)
            => Consumer Event m (Maybe (Tag b n))
parseText = tagName "{http://www.w3.org/2000/svg}text" textAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,la,x,y,dx,dy,rot,textlen) ->
    do let st hmaps = (parseStyles style hmaps) ++
                      (parsePA  pa  hmaps) ++
                      (cssStylesFromMap hmaps "text" (id1 ca) class_)
       insideText <- many (tContent (cpa,ca,gea,pa,class_,style,ext,tr,la,x,y,dx,dy,rot,textlen))
       return $ SubTree True (id1 ca)
                             (0, 0)
                             Nothing
                             Nothing
                             (\maps -> applyStyleSVG st maps)
                             insideText

tContent (cpa,ca,gea,pa,class_,style,ext,tr,la,x,y,dx,dy,rot,textlen)
         = choose
           [ parseTSpan  (cpa,ca,gea,pa,class_,style,ext,tr,la,x,y,dx,dy,rot,textlen),
             textContent (cpa,ca,gea,pa,class_,style,ext,tr,la,x,y,dx,dy,rot,textlen) ]


{-
-- text related data of pa (presentation attribute)

alignmentBaseline baselineShift dominantBaseline fontFamily
fntSize fontSizeAdjust fontStretch fontStyle fontVariant fontWeight
glyphOrientationHorizontal glyphOrientationVertical kerning letterSpacing
textAnchor textDecoration textRendering wordSpacing writingMode
-}

-- | Parse a string between the text tags:  \<text\>Hello\</text\>
textContent :: (MonadThrow m, InputConstraints b n, RealFloat n, Read n, Renderable (TT.Text n) b) =>
               (ConditionalProcessingAttributes,
                CoreAttributes,
                GraphicalEventAttributes,
                PresentationAttributes,
                Maybe Text,
                Maybe Text,
                Maybe Text,
                Maybe Text,
                Maybe Text,
                Maybe Text,
                Maybe Text,
                Maybe Text,
                Maybe Text,
                Maybe Text,
                Maybe Text) -> ConduitM Event o m (Maybe (Tag b n))
textContent (cpa,ca,gea,pa,class_,style,ext,tr,la,x,y,dx,dy,rot,textlen) =
  do t <- contentMaybe
     let st :: (Read a, RealFloat a, RealFloat n) => (HashMaps b n, ViewBox n) -> [(SVGStyle n a)]
         st (hmaps,_) = (parseStyles style hmaps) ++
                        (parsePA  pa  hmaps)

     let f :: (V b ~ V2, N b ~ n, RealFloat n, Read n, Typeable n, Renderable (TT.Text n) b)
           => (HashMaps b n, ViewBox n) -> Diagram b
         f (maps,(minx,miny,w,h)) = anchorText pa (maybe "" T.unpack t)
                                    -- fontWeight
                                    # scaleY (-1)
                                    # translate (r2 (p (minx,w) 0 x, p (miny,h) 0 y))
                                    # (applyTr (parseTr tr))
                                    # applyStyleSVG st (maps,(minx,miny,w,h))
                                    # maybe id (fontSize . local . read . T.unpack) (fntSize pa)
                                    # maybe id (font . T.unpack) (fontFamily pa)

     return (if isJust t then Just $ Leaf (id1 ca) mempty f
                         else Nothing)


{-<tspan
         sodipodi:role="line"
         id="tspan2173"
         x="1551.4218"
         y="1056.9836" /> -}

-------------------------------------------------------------------------------------------------
-- | Parse \<tspan\>, see <https://www.w3.org/TR/SVG/text.html#TSpanElement>
parseTSpan :: (MonadThrow m, InputConstraints b n, RealFloat n, Read n, Renderable (TT.Text n) b) =>
              (ConditionalProcessingAttributes,
               CoreAttributes,
               GraphicalEventAttributes,
               PresentationAttributes,
               Maybe Text,
               Maybe Text,
               Maybe Text,
               Maybe Text,
               Maybe Text,
               Maybe Text,
               Maybe Text,
               Maybe Text,
               Maybe Text,
               Maybe Text,
               Maybe Text) -> ConduitM Event o m (Maybe (Tag b n))
parseTSpan (cpa,ca,gea,pa,class_,style,ext,tr,la,x,y,dx,dy,rot,textlen) = tagName "{http://www.w3.org/2000/svg}tspan" tspanAttrs $
  \(cpa1,ca1,gea1,pa1,class1,style1,ext1,x1,y1,dx1,dy1,rot1,textlen1,lAdjust1,role) ->
    do t <- contentMaybe
       let st :: (Read a, RealFloat a, RealFloat n) => (HashMaps b n, ViewBox n) -> [(SVGStyle n a)]
           st (hmaps,_) = (parseStyles style hmaps) ++
                          (parseStyles style1 hmaps) ++
                          (parsePA  pa  hmaps) ++
                          (parsePA  pa1  hmaps) ++
                          (cssStylesFromMap hmaps "tspan" (id1 ca) class_)

       let f :: (V b ~ V2, N b ~ n, RealFloat n, Read n, Typeable n, Renderable (TT.Text n) b)
             => (HashMaps b n, ViewBox n) -> Diagram b
           f (maps,(minx,miny,w,h)) = anchorText pa (maybe "" T.unpack t)
                             # maybe id (fontSize . local . read . T.unpack) (pref (fntSize pa1) (fntSize pa))
                             # maybe id (font . T.unpack) (pref (fontFamily pa1) (fontFamily pa))
                             -- fontWeight
                             # scaleY (-1)
                             # translate (r2 (p (minx,w) 0 (pref x1 x), p (miny,h) 0 (pref y1 y)))
                             # (applyTr (parseTr tr))
                             # applyStyleSVG st (maps,(minx,miny,w,h))
       return $ Leaf (id1 ca) mempty f


anchorText :: (V b ~ V2, N b ~ n, RealFloat n, Read n, Typeable n, Renderable (TT.Text n) b)
           => PresentationAttributes -> String -> QDiagram b V2 n Any
anchorText pa txt = case anchor pa of 
    "start"   -> baselineText txt
    "middle"  -> text txt
    "end"     -> alignedText 1 0 txt -- TODO is this correct?
    "inherit" -> text txt -- TODO
  where
    anchor pa = maybe "start" T.unpack (textAnchor pa) -- see <https://www.w3.org/TR/SVG/text.html#TextAnchorProperty>


pref :: Maybe a -> Maybe a -> Maybe a
pref (Just x) b       = Just x
pref Nothing (Just y) = Just y
pref Nothing Nothing  = Nothing


--------------------------------------------------------------------------------------
-- Gradients
-------------------------------------------------------------------------------------

-- | Parse \<linearGradient\>, see <http://www.w3.org/TR/SVG/pservers.html#LinearGradientElement>
-- example: <linearGradient id="SVGID_2_" gradientUnits="userSpaceOnUse" x1="68.2461" y1="197.6797"
--           x2="52.6936" y2="237.5337" gradientTransform="matrix(1 0 0 -1 -22.5352 286.4424)">
parseLinearGradient :: (MonadThrow m, V b ~ V2, N b ~ n, RealFloat n) => Consumer Event m (Maybe (Tag b n))
parseLinearGradient = tagName "{http://www.w3.org/2000/svg}linearGradient" linearGradAttrs $
  \(ca,pa,xlink,class_,style,ext,x1,y1,x2,y2,gradientUnits,gradientTransform,spreadMethod) -> -- TODO gradientUnits
  do gs <- many gradientContent
     let stops = map getTexture $ concat $ map extractStops gs

     -- because of href we have to replace Nothing-attributes by attributes of referenced gradients
     -- see <http://www.w3.org/TR/SVG/pservers.html#RadialGradientElementHrefAttribute>
     let attributes = GA pa class_ style x1 y1 x2 y2 Nothing Nothing Nothing Nothing Nothing gradientUnits gradientTransform spreadMethod

     -- stops are lists of functions and everyone of these gets passed the same cssmap
     -- and puts them into a Grad constructor
     let f css attributes (minx,miny,w,h) stops =
           over (_LG . lGradTrans) (applyTr (parseTr gradientTransform))
           (mkLinearGradient (concat (map ($ css) stops)) -- (minx,miny,w,h) is the viewbox
                             ((p (minx,w) 0 x1) ^& (p (miny,h) 0 y1))
                             ((p (minx,w) 0 x2) ^& (p (miny,h) 0 y2))
                             (parseSpread spreadMethod))
     return $ Grad (id1 ca) (Gr (Diagrams.SVG.Attributes.fragment $ xlinkHref xlink) attributes Nothing stops f)

gradientContent = choose [parseStop, parseMidPointStop] -- parseSet,
   --   parseDesc, parseMetaData, parseTitle] -- descriptive Elements (rarely used here, so tested at the end)

-- | Parse \<radialGradient\>, see <http://www.w3.org/TR/SVG/pservers.html#RadialGradientElement>
parseRadialGradient :: (MonadThrow m, V b ~ V2, N b ~ n, RealFloat n) => Consumer Event m (Maybe (Tag b n))
parseRadialGradient = tagName "{http://www.w3.org/2000/svg}radialGradient" radialGradAttrs $ -- TODO gradientUnits
  \(ca,pa,xlink,class_,style,ext,cx,cy,r,fx,fy,gradientUnits,gradientTransform,spreadMethod) -> 
  do gs <- many gradientContent
     let stops = map getTexture $ concat $ map extractStops gs

     -- because of href we have to replace Nothing-attributes by attributes of referenced gradients
     -- see <http://www.w3.org/TR/SVG/pservers.html#RadialGradientElementHrefAttribute>
     let attributes = GA pa class_ style Nothing Nothing Nothing Nothing cx cy r fx fy gradientUnits gradientTransform spreadMethod

     let f css attributes (minx,miny,w,h) stops =
            over (_RG . rGradTrans) (applyTr (parseTr gradientTransform))
            (mkRadialGradient (concat (map ($ css) stops))
                              ((p (minx,w) (p (minx,w) 0 cx) fx) ^& -- focal point fx is set to cx if fx does not exist
                              (p (miny,h) (p (miny,h) 0 cy) fy))
                              0
                              ((p (minx,w) 0             cx) ^& 
                              (p (miny,h) 0             cy))
                              (p (minx,w) (0.5*(w-minx)) r) --TODO radius percentage relative to x or y?
                              (parseSpread spreadMethod))
     return $ Grad (id1 ca) (Gr (Diagrams.SVG.Attributes.fragment $ xlinkHref xlink) attributes Nothing stops f)

extractStops (SubTree b id1 wh viewBox ar f children) = concat (map extractStops children)
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
      return $ Stop (\hmaps -> mkStops [getStopTriple (p (0,1) 0 offset) (st hmaps)])

parseMidPointStop = tagName "{http://www.w3.org/2000/svg}midPointStop" stopAttrs $
   \(ca,pa,xlink,class_,style,offset) ->
   do let st hmaps = (parseStyles style empty3) ++
                     (parsePA pa empty3) ++
                     (cssStylesFromMap hmaps "midPointStop" (id1 ca) class_)
      return $ Stop (\hmaps -> mkStops [getStopTriple (p (0,1) 0 offset) (st hmaps)])

empty3 = (H.empty,H.empty,H.empty)

getStopTriple offset styles = (col colors, offset, opacity opacities)
  where col ((Fill c):_) = fromAlphaColour c
        col _ = white
        opacity ((FillOpacity x):_) = x
        opacity _ =  1
        colors = Prelude.filter isFill styles
        opacities = Prelude.filter isOpacity styles

isFill (Fill _) = True
isFill _        = False

isOpacity (FillOpacity _) = True
isOpacity _           = False

----------------------------------------------------------------------------------------------------
-- Fonts
----------------------------------------------------------------------------------------------------

parseFont :: (MonadThrow m, V b ~ V2, N b ~ n, RealFloat n, Renderable (DImage (N b) Embedded) b, Renderable (Path V2 n) b,
              Typeable b, Typeable n, Show n, Read n, Renderable (TT.Text n) b) => Consumer Event m (Maybe (Tag b n))
parseFont = tagName "{http://www.w3.org/2000/svg}font" fontAttrs $
  \(ca,pa,class_,style,ext,hOriginX,hOriginY,hAdvX,vOriginX,vOriginY,vAdvY) ->
  do gs <- many fontContent
     return $ FontTag $ FontData (id1 ca) hOriginX hOriginY (getN hAdvX) vOriginX vOriginY vAdvY 
                                 (fontf gs) (missingGlyph gs) (glyphs gs) (kernMap (kerns gs))
  where fontf gs        = (\(FF f) -> f) $ head $ Prelude.filter isFontFace gs
        missingGlyph gs = (\(GG g) -> g) $ head $ Prelude.filter isMissingGlyph gs
        glyphs gs       = H.fromList $ map toSvgGlyph     (Prelude.filter isGlyph gs)
        kerns gs = map (\(KK k) -> k) (Prelude.filter isKern gs)

        isGlyph (GG (Glyph glyphId g d _ _ _ _ unicode glyphName o a l)) = not (maybe False T.null unicode) ||
                                                                           not (maybe False T.null glyphName)
        isGlyph _        = False
        isMissingGlyph (GG (Glyph glyphId g d _ _ _ _ unicode glyphName o a l)) = (maybe False T.null unicode) &&
                                                                                  (maybe False T.null glyphName)
        isMissingGlyph _  = False
        isKern (KK k)     = True
        isKern _          = False
        isFontFace (FF f) = True
        isFontFace _      = False
        toSvgGlyph (GG (Glyph glyphId g d horizAdvX _ _ _ (Just unicode) glyphName o a l)) = (unicode,(glyphName,horizAdvX,d))

fontContent :: (MonadThrow m, InputConstraints b n, Read n, Show n, Renderable (TT.Text n) b) 
             => Consumer Event m (Maybe (FontContent b n))
fontContent = choose -- the likely most common are checked first
     [parseGlyph, parseHKern, parseFontFace, parseMissingGlyph, parseVKern]


parseFontFace :: (MonadThrow m, V b ~ V2, N b ~ n, Read n, RealFloat n, Renderable (DImage (N b) Embedded) b,
              Typeable b, Typeable n) => Consumer Event m (Maybe (FontContent b n))
parseFontFace = tagName "{http://www.w3.org/2000/svg}font-face" fontFaceAttrs $
  \(ca,fontFamily,fontStyle,fontVariant,fontWeight,fontStretch,fontSize,unicodeRange,unitsPerEm,panose1,
    stemv,stemh,slope,capHeight,xHeight,accentHeight,ascent,descent,widths,bbox,ideographic,alphabetic,mathematical,
    hanging,vIdeographic,vAlphabetic,vMathematical,vHanging,underlinePosition,underlineThickness,strikethroughPosition,
    strikethroughThickness,overlinePosition,overlineThickness) ->
  do return $ FF $ FontFace fontFamily fontStyle fontVariant fontWeight fontStretch fontSize unicodeRange unitsPerEm panose1
                            stemv stemh slope capHeight xHeight accentHeight ascent descent widths (parseBBox bbox) ideographic
                            alphabetic mathematical hanging vIdeographic vAlphabetic  vMathematical vHanging underlinePosition 
                            underlineThickness strikethroughPosition strikethroughThickness overlinePosition overlineThickness


parseMissingGlyph :: (MonadThrow m, V b ~ V2, N b ~ n, RealFloat n, Read n, Renderable (DImage (N b) Embedded) b,
              Typeable b, Typeable n) => Consumer Event m (Maybe (FontContent b n))
parseMissingGlyph = tagName "{http://www.w3.org/2000/svg}missing-glyph" missingGlyphAttrs $
  \(ca,pa,class_,style,d,horizAdvX,vertOriginX,vertOriginY,vertAdvY) ->
  do return $ GG $ Glyph (id1 ca) (Leaf (id1 ca) mempty mempty) Nothing
                         (getN horizAdvX) (getN vertOriginX) (getN vertOriginY) (getN vertAdvY)
                         Nothing Nothing Nothing Nothing Nothing


parseGlyph :: (MonadThrow m, V b ~ V2, N b ~ n, RealFloat n, Read n, Renderable (DImage (N b) Embedded) b,
              Renderable (Path V2 n) b, Show n, Typeable b, Typeable n, Renderable (TT.Text n) b) 
           => Consumer Event m (Maybe (FontContent b n))
parseGlyph = tagName "{http://www.w3.org/2000/svg}glyph" glyphAttrs $
  \(ca,pa,class_,style,d,horizAdvX,vertOriginX,vertOriginY,vertAdvY,unicode,glyphName,orientation,arabicForm,lang) ->
  do gs <- many gContent
     let st hmaps = parseStyles style hmaps
     let sub = SubTree True (id1 ca) (0,0) Nothing Nothing (\maps -> (applyStyleSVG st maps)) (reverse gs)
     return $ GG $ Glyph (id1 ca) sub d (getN horizAdvX) (getN vertOriginX) (getN vertOriginY) (getN vertAdvY)
                         unicode glyphName orientation arabicForm lang

getN = maybe 0 (read . T.unpack)

parseHKern :: (MonadThrow m, V b ~ V2, N b ~ n, RealFloat n, Read n, Typeable b, Typeable n) => Consumer Event m (Maybe (FontContent b n))
parseHKern = tagName "{http://www.w3.org/2000/svg}hkern" kernAttrs $
  \(ca,u1,g1,u2,g2,k) ->
  do return $ KK $ Kern HKern (charList u1) (charList g1) (charList u2) (charList g2) (getN k)

parseVKern :: (MonadThrow m, V b ~ V2, N b ~ n, RealFloat n, Read n, Typeable b, Typeable n) => Consumer Event m (Maybe (FontContent b n))
parseVKern = tagName "{http://www.w3.org/2000/svg}vkern" kernAttrs $
  \(ca,u1,g1,u2,g2,k) ->
  do return $ KK $ Kern VKern (charList u1) (charList g1) (charList u2) (charList g2) (getN k)

charList :: Maybe Text -> [Text]
charList str = maybe [] (T.splitOn ",") str

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

skipArbitraryTag :: (MonadThrow m, InputConstraints b n, Renderable (TT.Text n) b, Read n) => Consumer Event m (Maybe (Tag b n))
skipArbitraryTag = do t <- ignoreAnyTreeContent
                      if isJust t then return (Just $ Leaf (Just "") mempty mempty)
                                  else return Nothing

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
{-  Maybe we implement it one day

parseMetaData :: (MonadThrow m, V b ~ V2, N b ~ n, RealFloat n) => Consumer Event m (Maybe (Tag b n))
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
-}
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
