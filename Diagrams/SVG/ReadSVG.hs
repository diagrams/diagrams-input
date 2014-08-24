{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExistentialQuantification #-}

--------------------------------------------------------------------
-- |
-- Module    : Diagrams.SVG.ReadSVG
-- Copyright : (c) 2014 Tillmann Vogt <tillk.vogt@googlemail.com>
-- License   : BSD3
--
-- Maintainer: diagrams-discuss@googlegroups.com
-- Stability : stable
-- Portability: portable

-----------------------------------------------------------------------------------------------------

module Diagrams.SVG.ReadSVG
    (
    -- * Main functions
      Width(..)
    , Height(..)
    , PreserveAR(..)
    , AlignSVG(..)
    , Place(..)
    , MeetOrSlice(..)
    , readSVGFile
    , preserveAspectRatio
    , nodes
    , insertRefs
    , clipByRef
    , evalPath
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
    ) where

import Control.Monad.Catch
import Control.Monad.Trans.Resource
import Data.Conduit
--import qualified Data.Conduit.List as C
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Text as T
import Data.Text(Text(..))
import Data.Text.Encoding
import Data.XML.Types
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.Ellipse
import Diagrams.TwoD.Size
import Diagrams.TwoD.Types
import Diagrams.SVG.Arguments
import Diagrams.SVG.Attributes (applyTr, parseTr, applyStyleSVG, parseStyles, parseDouble, parseMaybeDouble, parsePoints, parsePA, CoreAttributes(..), ConditionalProcessingAttributes(..), DocumentEventAttributes(..), GraphicalEventAttributes(..), PresentationAttributes(..), XlinkAttributes(..), FilterPrimitiveAttributes(..), NameSpaces(..), PreserveAR(..), AlignSVG(..), Place(..), MeetOrSlice(..), SVGStyle(..), p, cssStylesFromMap, parseTempl, parseIRI)
import Diagrams.SVG.Path (commands, commandsToTrails, PathCommand(..))
import Diagrams.SVG.Tree 
import Filesystem.Path (FilePath)
import Text.XML.Stream.Parse hiding (parseText)
import Text.CSS.Parse (parseBlocks)
import Prelude hiding (FilePath)
import Data.Tuple.Select
import Debug.Trace

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
--    let min = PAR (AlignXY 0.5 0) Meet
--    let mid = PAR (AlignXY 0.5 0.5) Meet
--    let max = PAR (AlignXY 1 1) Meet
--    diagramFromSVG <- readSVGFile mid 100 100 \"svgs/web.svg\"
--    mainWith $ diagramFromSVG
-- @
--
readSVGFile :: PreserveAR -> Width -> Height -> FilePath -> IO (Diagram B R2)
readSVGFile preserveAR width height fp =
  do tree <- runResourceT $ parseFile def fp $$ force "svg tag required" parseSVG 
              -- (C.map stripNamespace parseSVG)
     let (ns,css,grad) = -- Debug.Trace.trace ("tree" ++ show (nodes tree) ++ "\n") $
                         nodes ([],[],[]) tree
     let nmap    = H.fromList ns -- needed because of the use-tag and clipPath
     let cssmap  = H.fromList css -- CSS inside the <defs> tag
     let gradmap = H.fromList $ map (\(id1,f) -> (id1, f cssmap)) grad
     let image = (scaleY (-1)) (insertRefs (nmap,cssmap,gradmap) tree) -- insert references from hashmaps into tree
     return (preserveAspectRatio width height preserveAR image)

type Width = Double
type Height = Double

-- | According to <http://www.w3.org/TR/SVG11/coords.html#PreserveAspectRatioAttribute>
-- To Do: implement slice
preserveAspectRatio :: Width -> Height -> PreserveAR -> Diagram B R2 -> Diagram B R2
preserveAspectRatio newWidth newHeight (PAR alignXY Meet) image
   | aspectRatio < newAspectRatio = xPlace alignXY image
   | otherwise                    = yPlace alignXY image
  where w = Diagrams.TwoD.Size.width  image
        h = Diagrams.TwoD.Size.height image
        aspectRatio = w / h
        newAspectRatio = newWidth / newHeight
        scaX = newHeight / h
        scaY = newWidth / w
        xPlace (AlignXY x y) i = i # scale scaX # alignBL # translateX ((newWidth  - w*scaX)*x) -- # showOrigin
        yPlace (AlignXY x y) i = i # scale scaY # alignBL # translateY ((newHeight - h*scaY)*y) -- # showOrigin

------------------------------------------------------------------------------------------------------------

evalGrad cssmap (Grad id1 tex) = (id1, tex cssmap)

-- lookup a diagram
lookUp hmap i | isJust l  = fromJust l
              | otherwise = Leaf Nothing Nothing mempty mempty -- an empty diagram if we can't find the id
  where l = H.lookup i hmap

insertRefs :: (H.HashMap Text Tag, H.HashMap Text Attrs, H.HashMap Text Texture) -> Tag -> Diagram B R2
insertRefs maps (Leaf id1 clipRef path f) = f maps # clipByRef (sel1 maps) clipRef
insertRefs maps (Grad _ _) = mempty
insertRefs maps (Stop f)   = mempty
insertRefs maps (Reference selfId id1 clipRef f) = (f maps) $ insertRefs maps (lookUp (sel1 maps) (fragment id1))
                                                    # clipByRef (sel1 maps) clipRef

insertRefs maps (SubTree True id1 clipRef f children) = (f maps) (mconcat (map (insertRefs maps) children))
                                                         # clipByRef (sel1 maps) clipRef

insertRefs maps (SubTree False _ _ _ _) = mempty

clipByRef hmap ref | isJust l  = -- Debug.Trace.trace (show $ evalPath hmap (fromJust l)) $
                                 clipBy $ evalPath hmap (fromJust l)
                   | otherwise = -- Debug.Trace.trace (show ref) $
                                 id
  where l = H.lookup (fragment ref) hmap

evalPath :: H.HashMap Text Tag -> Tag -> Path R2
evalPath hmap (Leaf             id1 clipRef path diagram)   = path
evalPath hmap (Reference selfId id1 clipRef f)              = evalPath hmap (lookUp hmap (fragment id1))
evalPath hmap (SubTree _            id1 clipRef f children) = mconcat (map (evalPath hmap) children)
evalPath hmap _ = mempty

fragment x = fromMaybe T.empty $ fmap snd (parseTempl parseIRI x) -- look only for the text after "#"

-------------------------------------------------------------------------------------
-- Basic SVG structure

-- | Parse \<svg\>, see <http://www.w3.org/TR/SVG/struct.html#SVGElement>
parseSVG :: MonadThrow m => Sink Event m (Maybe Tag)
parseSVG = tagName "svg" svgAttrs $
   \(cpa,ca,gea,pa,class_,style,ext,x,y,w,h,view,ar,zp,ver,baseprof,cScripT,cStyleT,xmlns,xml) ->
   do gs <- many svgContent
      let st (ns,css,grad) = (parseStyles style grad) ++
                             (parsePA     pa    grad) ++
                             (cssStylesFromMap css grad "svg" (id1 ca) class_)
      return $ SubTree True (id1 ca)
                            (clipPath pa)
                            (applyStyleSVG st)
                            (reverse gs)

svgContent = choose -- the likely most common are checked first
     [parsePath, parseRect, parseCircle, parseEllipse, parseLine, parsePolyLine, parsePolygon, -- shape elements
      parseG, parseDefs, parseSymbol, parseUse, -- structural elements
      parseClipPath, parseText, parsePattern, parseImage, parseSwitch, parseSodipodi,
      parseDesc, parseMetaData, parseTitle] -- descriptive Elements

---------------------------------------------------------------------------
-- | Parse \<g\>, see <http://www.w3.org/TR/SVG/struct.html#GElement>
parseG :: MonadThrow m => Consumer Event m (Maybe Tag)
parseG = tagName "g" gAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr) ->
   do insideGs <- many gContent
      let st (ns,css,grad) = (parseStyles style grad) ++
                             (parsePA     pa    grad) ++
                             (cssStylesFromMap css grad "g" (id1 ca) class_)
      return $ SubTree True (id1 ca)
                            (clipPath pa)
                            ( (applyTr (parseTr tr)) . (applyStyleSVG st) )
                            (reverse insideGs)

gContent = choose -- the likely most common are checked first
     [parsePath, parseRect, parseCircle, parseEllipse, parseLine, parsePolyLine, parsePolygon, -- shape elements
      parseG, parseDefs, parseStyle, parseSymbol, parseUse, -- structural elements
      parseLinearGradient, parseRadialGradient, parseClipPath, parseImage, parseText, parsePattern, parseSwitch, parsePerspective,
      parseDesc, parseMetaData, parseTitle] -- descriptive Elements

---------------------------------------------------------------------------
-- | Parse \<defs\>, see <http://www.w3.org/TR/SVG/struct.html#DefsElement>
parseDefs :: MonadThrow m => Consumer Event m (Maybe Tag)
parseDefs = tagName "defs" gAttrs $
   \(cpa,ca,gea,pa,class_,style,ext,tr) ->
   do insideDefs <- many gContent
      let st (ns,css,grad) = (parseStyles style grad) ++
                             (parsePA     pa    grad) ++
                             (cssStylesFromMap css grad "defs" (id1 ca) class_)
      return $ SubTree False (id1 ca)
                             (clipPath pa)
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

parseStyle :: MonadThrow m => Consumer Event m (Maybe Tag)
parseStyle = tagName "style" sAttrs $
   \(ca,type_,media,title) ->
   do insideStyle <- content
      let blocks = parseBlocks insideStyle -- parseBlocks :: Text -> Either String [CssBlock]
      let cssBlocks = case blocks of
                   Left err -> []
                   Right st -> st
      return $ StyleTag cssBlocks -- type CssBlock = (Text, [(Text, Text)]) = (selector, [(attribute, value)])

-----------------------------------------------------------------------------------
-- | Parse \<symbol\>, see <http://www.w3.org/TR/SVG/struct.html#SymbolElement>
parseSymbol :: MonadThrow m => Consumer Event m (Maybe Tag)
parseSymbol = tagName "symbol" symbolAttrs $
   \(ca,gea,pa,class_,style,ext,ar,viewbox) ->
   do insideSym <- many symbolContent
      let st (ns,css,grad) = (parseStyles style grad) ++
                             (parsePA     pa    grad) ++
                             (cssStylesFromMap css grad "symbol" (id1 ca) class_)
      return $ SubTree False (id1 ca)
                             (clipPath pa)
                             (applyStyleSVG st)
                             (reverse insideSym)

symbolContent = choose [ parsePath ]

-----------------------------------------------------------------------------------
-- | Parse \<use\>, see <http://www.w3.org/TR/SVG/struct.html#UseElement>
parseUse = tagName "use" useAttrs
   $ \(ca,cpa,gea,pa,xlink,class_,style,ext,tr,x,y,w,h) ->
   do insideUse <- many useContent
      let st (nss,css,grad) = (parseStyles style grad) ++
                              (parsePA     pa    grad) ++
                              (cssStylesFromMap css grad "use" (id1 ca) class_)
      return $ Reference (id1 ca) (xlinkHref xlink) (clipPath pa)
                         ( (translate (r2 (p x, p y))) . (applyTr (parseTr tr)) . (applyStyleSVG st) )

useContent = choose [parseDesc,parseTitle] -- descriptive elements

--------------------------------------------------------------------------------------
-- | Parse \<switch\>, see <http://www.w3.org/TR/SVG/struct.html#SwitchElement>
parseSwitch = tagName "switch" switchAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr) ->
   do insideSwitch <- many switchContent
      return $ Leaf (id1 ca) (clipPath pa) mempty mempty

switchContent = choose [parsePath, parseRect, parseCircle, parseEllipse, parseLine, parsePolyLine, parsePolygon]

----------------------------------------------------------------------------------------
-- descriptive elements
------------------------------------------------------o	----------------------------------
-- | Parse \<desc\>, see <http://www.w3.org/TR/SVG/struct.html#DescriptionAndTitleElements>
parseDesc :: MonadThrow m => Consumer Event m (Maybe Tag)
parseDesc = tagName "desc" descAttrs
   $ \(ca,class_,style) ->
   do desc <- content
      return $ Leaf (id1 ca) Nothing mempty mempty

-- | Parse \<title\>, see <http://www.w3.org/TR/SVG/struct.html#DescriptionAndTitleElements>
parseTitle = tagName "title" descAttrs
   $ \(ca,class_,style) ->
   do title <- content
      return $ Leaf (id1 ca) Nothing mempty mempty

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
parseMetaData = tagName "metadata" ignoreAttrs
   $ \_ ->
   do meta <- many metaContent
      return $ Leaf Nothing Nothing mempty mempty

metaContent = choose [parseRDF] -- extend if needed

parseRDF = tagName "{http://www.w3.org/1999/02/22-rdf-syntax-ns#}RDF" ignoreAttrs
          $ \_ ->
          do c <- parseWork
             return $ Leaf Nothing Nothing mempty mempty

parseWork = tagName "{http://creativecommons.org/ns#}Work" ignoreAttrs
   $ \_ ->
   do c <- many workContent
      return $ Leaf Nothing Nothing mempty mempty

workContent = choose [parseFormat, parseType, parseRDFTitle] -- extend if needed

parseFormat = tagName "{http://purl.org/dc/elements/1.1/}format" ignoreAttrs
   $ \_ ->
   do c <- content
      return $ Leaf Nothing Nothing mempty mempty

parseType = tagName "{http://purl.org/dc/elements/1.1/}type" ignoreAttrs
   $ \_ ->
   do c <- content
      return $ Leaf Nothing Nothing mempty mempty

parseRDFTitle = tagName "{http://purl.org/dc/elements/1.1/}title" ignoreAttrs
   $ \_ ->
   do c <- content
      return $ Leaf Nothing Nothing mempty mempty

test :: Data.XML.Types.Name
test = "{http://www.w3.org/1999/02/22-rdf-syntax-ns#}RDF"

------------------------------------
-- inkscape / sodipodi tags
------------------------------------

parseSodipodi = tagName "{http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd}namedview" namedViewAttrs
   $ \(pc,bc,bo,ot,gt,gut,po,ps,ww,wh,id1,sg,zoom,cx,cy,wx,wy,wm,cl) ->
   do c <- parseGrid
      return $ Leaf (Just "") Nothing mempty mempty

--    <inkscape:grid
--       type="xygrid"
--       id="grid5177" />
parseGrid = tagName "{http://www.inkscape.org/namespaces/inkscape}grid" ignoreAttrs
   $ \_ ->
   do c <- content
      return $ Leaf Nothing Nothing mempty mempty

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
   do return $ Leaf (Just "") Nothing mempty mempty

-----------------------------------------------------------------------------------
-- | Parse \<rect\>,  see <http://www.w3.org/TR/SVG11/shapes.html#RectElement>
parseRect :: MonadThrow m => Consumer Event m (Maybe Tag)
parseRect = tagName "rect" rectAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,ar,tr,x,y,w,h,rx,ry) -> do
    let st (ns,css,grad) = (parseStyles style grad) ++
                           (parsePA     pa    grad) ++
                           (cssStylesFromMap css grad "rect" (id1 ca) class_)
    return $ Leaf (id1 ca)
                  (clipPath pa)
                  (path x y w h rx ry tr)
                  (\maps -> (path x y w h rx ry tr # stroke # lwL 0 # applyStyleSVG st maps))

  where path x y w h rx ry tr = (rRect (p w) (p h) (p rx) (p ry)) # alignBL 
                                                                  # applyTr (parseTr tr)
                                                                  # translate (r2 (p x, p y))
        rRect pw ph prx pry | prx == 0 && pry == 0 = rect pw ph :: Path R2
                            | otherwise = roundedRect pw ph (if prx == 0 then pry else prx) :: Path R2

---------------------------------------------------------------------------------------------------
-- | Parse \<circle\>,  see <http://www.w3.org/TR/SVG11/shapes.html#CircleElement>
parseCircle :: MonadThrow m => Consumer Event m (Maybe Tag)
parseCircle = tagName "circle" circleAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,r,cx,cy) -> do
    let st (ns,css,grad) = (parseStyles style grad) ++
                           (parsePA     pa    grad) ++
                           (cssStylesFromMap css grad "circle" (id1 ca) class_)
    return $ Leaf (id1 ca)
                  (clipPath pa)
                  (path cx cy r tr)
                  (\maps -> (path cx cy r tr # stroke # lwL 0 # applyStyleSVG st maps))

  where path cx cy r tr = circle (p r) # applyTr (parseTr tr) # translate (r2 (p cx, p cy))

---------------------------------------------------------------------------------------------------
-- | Parse \<ellipse\>,  see <http://www.w3.org/TR/SVG11/shapes.html#EllipseElement>
parseEllipse :: MonadThrow m => Consumer Event m (Maybe Tag)
parseEllipse = tagName "ellipse" ellipseAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,rx,ry,cx,cy) -> do
    let st (ns,css,grad) = (parseStyles style grad) ++
                           (parsePA     pa    grad) ++
                           (cssStylesFromMap css grad "ellipse" (id1 ca) class_)
    return $ Leaf (id1 ca)
                  (clipPath pa)
                  (path cx cy rx ry tr)
                  (\maps -> (path cx cy rx ry tr # stroke # lwL 0 # applyStyleSVG st maps))

  where path cx cy rx ry tr = ellipseXY (p rx) (p ry) # applyTr (parseTr tr) # translate (r2 (p cx, p cy))

---------------------------------------------------------------------------------------------------
-- | Parse \<line\>,  see <http://www.w3.org/TR/SVG11/shapes.html#LineElement>
parseLine :: MonadThrow m => Consumer Event m (Maybe Tag)
parseLine = tagName "line" lineAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,x1,y1,x2,y2) -> do
    let st (ns,css,grad) = (parseStyles style grad) ++
                           (parsePA     pa    grad) ++
                           (cssStylesFromMap css grad "line" (id1 ca) class_)
    return $ Leaf (id1 ca)
                  (clipPath pa)
                  (path x1 y1 x2 y2 tr)
                  (\maps -> (path x1 y1 x2 y2 tr # stroke # applyStyleSVG st maps))

  where path x1 y1 x2 y2 tr =
               fromSegments [ straight (r2 ((p x2) - (p x1), (p y2) - (p y1))) ]
               # applyTr (parseTr tr)
               # translate (r2 (p x1, p y1))

---------------------------------------------------------------------------------------------------
-- | Parse \<polyline\>,  see <http://www.w3.org/TR/SVG11/shapes.html#PolylineElement>
parsePolyLine :: MonadThrow m => Consumer Event m (Maybe Tag)
parsePolyLine = tagName "polyline" polygonAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,points) -> do
    let st (ns,css,grad) = (parseStyles style grad) ++
                           (parsePA     pa    grad) ++
                           (cssStylesFromMap css grad "polyline" (id1 ca) class_)
    let ps = parsePoints (fromJust points)
    return $ Leaf (id1 ca)
                  (clipPath pa)
                  (path ps tr)
                  (\maps -> (dia  ps tr # lwL 0 # applyStyleSVG st maps))

  where path ps tr = fromVertices (map p2 ps) # applyTr (parseTr tr)
                                              # translate (r2 (head ps))

        dia  ps tr = fromVertices (map p2 ps) # strokeLine
                                              # applyTr (parseTr tr)
                                              # translate (r2 (head ps))

--------------------------------------------------------------------------------------------------
-- | Parse \<polygon\>,  see <http://www.w3.org/TR/SVG11/shapes.html#PolygonElement>
parsePolygon :: MonadThrow m => Consumer Event m (Maybe Tag)
parsePolygon = tagName "polygon" polygonAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,points) -> do
    let st (ns,css,grad) = (parseStyles style grad) ++
                           (parsePA     pa    grad) ++
                           (cssStylesFromMap css grad "polygon" (id1 ca) class_)
    let ps = parsePoints (fromJust points)
    return $ Leaf (id1 ca)
                  (clipPath pa)
                  (path  ps tr)
                  (\maps -> (dia   ps tr # lwL 0 # applyStyleSVG st maps))

  where path ps tr = fromVertices (map p2 ps) # applyTr (parseTr tr)
                                              # translate (r2 (head ps))

        dia  ps tr = fromVertices (map p2 ps) # closeLine
                                              # strokeLoop
                                              # applyTr (parseTr tr)
                                              # translate (r2 (head ps))

--------------------------------------------------------------------------------------------------
-- | Parse \<path\>,  see <http://www.w3.org/TR/SVG11/paths.html#PathElement>
parsePath :: MonadThrow m => Consumer Event m (Maybe Tag)
parsePath = tagName "path" pathAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,d,pathLength) -> do
    let st (ns,css,grad) = (parseStyles style grad) ++
                           (parsePA     pa    grad) ++
                           (cssStylesFromMap css grad "path" (id1 ca) class_)
    return $ Leaf (id1 ca)
                  (clipPath pa)
                  (path d tr)
                  (\maps -> (path d tr # stroke # lwL 0 # applyStyleSVG st maps))

  where path d tr = (mconcat $ commandsToTrails $ commands d) # applyTr (parseTr tr)

-------------------------------------------------------------------------------------------------
-- | Parse \<clipPath\>, see <http://www.w3.org/TR/SVG/masking.html#ClipPathElement>
parseClipPath :: MonadThrow m => Consumer Event m (Maybe Tag)
parseClipPath = tagName "clipPath" clipPathAttrs $
  \(cpa,ca,pa,class_,style,ext,ar,viewbox) -> do
    insideClipPath <- many clipPathContent
    let st (ns,css,grad) = (parseStyles style grad) ++
                           (parsePA     pa    grad) ++
                           (cssStylesFromMap css grad "clipPath" (id1 ca) class_)
    return $ SubTree False (id1 ca) (clipPath pa) (applyStyleSVG st) (reverse insideClipPath)

clipPathContent = choose [parsePath, parseRect, parseCircle, parseEllipse, parseLine, parsePolyLine, parsePolygon,
                          parseText, parseUse]

--------------------------------------------------------------------------------------
-- | Parse \<image\>, see <http://www.w3.org/TR/SVG/struct.html#ImageElement>
parseImage :: MonadThrow m => Consumer Event m (Maybe Tag)
parseImage = tagName "image" imageAttrs $
  \(ca,cpa,gea,xlink,pa,class_,style,ext,ar,tr,x,y,w,h) ->
  do return $ Leaf (id1 ca) (clipPath pa) mempty mempty

-- | Parse \<text\>, see <http://www.w3.org/TR/SVG/text.html#TextElement>
parseText :: MonadThrow m => Consumer Event m (Maybe Tag)
parseText = tagName "text" textAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,la,x,y,dx,dy,rot,textlen) ->
  do t <- orE contentMaybe parseTSpan
     return $ Leaf (id1 ca) (clipPath pa) mempty mempty

{-<tspan
         sodipodi:role="line"
         id="tspan2173"
         x="1551.4218"
         y="1056.9836" /> -}

parseTSpan = tagName "tspan" tspanAttrs $
   \(role,id_,x,y) ->
   do return ""

--------------------------------------------------------------------------------------
-- Gradients

-- > gradient = mkLinearGradient stops ((-0.5) ^& 0) (0.5 ^& 0) GradPad
-- > sq1 = square 1 # fillTexture  gradient

-- | Parse \<linearGradient\>, see <http://www.w3.org/TR/SVG/pservers.html#LinearGradientElement>
-- example: <linearGradient id="SVGID_2_" gradientUnits="userSpaceOnUse" x1="68.2461" y1="197.6797"
--           x2="52.6936" y2="237.5337" gradientTransform="matrix(1 0 0 -1 -22.5352 286.4424)">
parseLinearGradient :: MonadThrow m => Consumer Event m (Maybe Tag)
parseLinearGradient = tagName "linearGradient" linearGradAttrs $
  \(cpa,ca,pa,xlink,class_,style,ext,x1,y1,x2,y2,gradientUnits,gradientTransform,spreadMethod) ->
  do gs <- many gradientContent
     let stops :: [CSSMap -> [GradientStop]]
         stops = map getTexture $ concat $ map extractStops gs
     -- stops are lists of functions and everyone of these gets passed the same (nodemap,gradmap)
     -- and puts them into a Grad constructor
     return $ Grad (id1 ca) (\css -> (mkLinearGradient (concat (map ($ css) stops))
                                                       ((parseMaybeDouble x1) ^& (parseMaybeDouble y1))
                                                       ((parseMaybeDouble x2) ^& (parseMaybeDouble y2)) GradPad)  )

gradientContent = choose
     [parseStop, parseSet,
      parseDesc, parseMetaData, parseTitle] -- descriptive Elements (rarely used here, so tested at the end)

-- | Parse \<radialGradient\>, see <http://www.w3.org/TR/SVG/pservers.html#RadialGradientElement>
parseRadialGradient :: MonadThrow m => Consumer Event m (Maybe Tag)
parseRadialGradient = tagName "radialGradient" radialGradAttrs $
  \(cpa,ca,pa,xlink,class_,style,ext,cx,cy,r,fx,fy,gradientUnits,gradientTransform,spreadMethod) ->
  do gs <- many gradientContent
     let stops = map getTexture $ concat $ map extractStops gs
     return $ Grad (id1 ca) (\css -> (mkRadialGradient (concat (map ($ css) stops))
                                                       ((parseMaybeDouble cx) ^& (parseMaybeDouble cy))
                                                       (parseMaybeDouble r)
                                                       (0 ^& 0) 0 GradPad)  )

extractStops :: Tag -> [Tag]
extractStops (SubTree b id1 clipRef f children) = concat (map extractStops children)
extractStops (Stop stops) = [Stop stops]
extractStops _ = []

getTexture (Stop stops) = stops

-- | Parse \<set\>, see <http://www.w3.org/TR/SVG/animate.html#SetElement>
parseSet = tagName "set" setAttrs $
   \(ca,pa,xlink) ->
   do return $ Leaf (id1 ca) Nothing mempty mempty -- "set" ignored so far

-- | Parse \<stop\>, see <http://www.w3.org/TR/SVG/pservers.html#StopElement>
--  e.g. <stop  offset="0.4664" style="stop-color:#000000;stop-opacity:0.8"/>
parseStop = tagName "stop" stopAttrs $
   \(ca,pa,xlink,class_,style,offset) ->
   do let st css = Debug.Trace.trace (show style ++ show (parseStyles style H.empty)) $
                   (parseStyles style H.empty) ++
                   (parsePA     pa    H.empty) ++
                   (cssStylesFromMap  css H.empty "stop" (id1 ca) class_)
      return $ Stop (\css -> mkStops [getStopTriple (parseMaybeDouble offset) (st css)])

-- (An opaque color, a stop fraction, an opacity).
-- mkStops :: [(Colour Double, Double, Double)] -> [GradientStop]
-- mkStops [(gray, 0, 1), (white, 0.5, 1), (purple, 1, 1)]

getStopTriple offset styles = Debug.Trace.trace (show styles ++ show (col c, offset, opacity o)) (col c, offset, opacity o)
  where col [Fill x] = x
        col _ = white
        opacity [FillOpacity x] = x
        opacity _ =  1
        c = Prelude.filter isFill styles
        o = Prelude.filter isOpacity styles

isFill (Fill _) = True
isFill _        = False

isOpacity (FillOpacity _) = True
isOpacity _           = False

-- | A gradient stop contains a color and fraction (usually between 0 and 1)
--data GradientStop = GradientStop
--     { _stopColor    :: SomeColor
--     , _stopFraction :: Double}

--------------------------------------------------------------------------------------
-- sceletons

-- | Parse \<pattern\>, see <http://www.w3.org/TR/SVG/pservers.html#PatternElement>
parsePattern :: MonadThrow m => Consumer Event m (Maybe Tag)
parsePattern = tagName "pattern" patternAttrs $
  \(cpa,ca,pa,class_,style,ext,view,ar,x,y,w,h,pUnits,pCUnits,pTrans) ->
  do insideSym <- many patternContent
     return $ Leaf (id1 ca) Nothing mempty mempty

patternContent = choose [parseImage]

-- | Parse \<filter\>, see <http://www.w3.org/TR/SVG/filters.html#FilterElement>
parseFilter :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFilter = tagName "filter" filterAttrs $
  \(ca,pa,xlink,class_,style,ext,x,y,w,h,filterRes,filterUnits,primUnits) ->
  do insideSym <- many filterContent
     return $ Leaf (id1 ca) Nothing mempty mempty

filterContent = choose [parseDesc,parseTitle, -- descriptive Elements
    parseFeBlend,parseFeColorMatrix,parseFeComponentTransfer,parseFeComposite,parseFeConvolveMatrix, -- filter primitive elments
    parseFeDiffuseLighting,parseFeDisplacementMap,parseFeFlood,parseFeGaussianBlur,parseFeImage,
    parseFeMerge,parseFeMorphology,parseFeOffset,parseFeSpecularLighting,parseFeTile,parseFeTurbulence]

--------------------------------------------------------------------------------------
-- filter primitives (currently only sceletons)
--------------------------------------------------------------------------------------

parseFeBlend :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeBlend = tagName "feBlend" feBlendAttrs $
   \(ca,pa,fpa,class_,style,in1,in2,mode) ->
   do return $ Leaf (id1 ca) Nothing mempty mempty

parseFeColorMatrix :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeColorMatrix = tagName "feColorMatrix" feColorMatrixAttrs $
   \(ca,pa,fpa,class_,style,in1,type1,values) ->
   do return $ Leaf (id1 ca) Nothing mempty mempty

parseFeComponentTransfer :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeComponentTransfer = tagName "feComponentTransfer" feComponentTransferAttrs $
   \(ca,pa,fpa,class_,style,in1) ->
   do return $ Leaf (id1 ca) Nothing mempty mempty

parseFeComposite :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeComposite = tagName "feComposite" feCompositeAttrs $
   \(ca,pa,fpa,class_,style,in1,in2,operator,k1,k2,k3,k4) ->
   do return $ Leaf (id1 ca) Nothing mempty mempty

parseFeConvolveMatrix :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeConvolveMatrix = tagName "feConvolveMatrix" feConvolveMatrixAttrs $
   \(ca,pa,fpa,class_,style,order,km,d,bias,tx,ty,em,ku,par) ->
   do return $ Leaf (id1 ca) Nothing mempty mempty

parseFeDiffuseLighting :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeDiffuseLighting = tagName "feDiffuseLighting" feDiffuseLightingAttrs $
   \(ca,pa,fpa,class_,style,in1,surfaceScale,diffuseConstant,kuLength) ->
   do return $ Leaf (id1 ca) Nothing mempty mempty

parseFeDisplacementMap :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeDisplacementMap = tagName "feDisplacementMap" feDisplacementMapAttrs $
   \(ca,pa,fpa,class_,style,in1,in2,sc,xChan,yChan) ->
   do return $ Leaf (id1 ca) Nothing mempty mempty

parseFeFlood :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeFlood = tagName "feFlood" feFloodAttrs $
   \(ca,pa,fpa,class_,style) ->
   do return $ Leaf (id1 ca) Nothing mempty mempty

parseFeGaussianBlur :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeGaussianBlur = tagName "feGaussianBlur" feGaussianBlurAttrs $
   \(ca,pa,fpa,class_,style,in1,stdDeviation) ->
   do return $ Leaf (id1 ca) Nothing mempty mempty

parseFeImage :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeImage = tagName "feImage" feImageAttrs $
   \(ca,pa,fpa,xlibk,class_,style,ext,par) ->
   do return $ Leaf (id1 ca) Nothing mempty mempty

parseFeMerge :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeMerge = tagName "feMerge" feMergeAttrs $
   \(ca,pa,fpa,class_,style) ->
   do return $ Leaf (id1 ca) Nothing mempty mempty

parseFeMorphology :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeMorphology = tagName "feMorphology" feMorphologyAttrs $
   \(ca,pa,fpa,class_,style,in1,operator,radius) ->
   do return $ Leaf (id1 ca) Nothing mempty mempty

parseFeOffset :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeOffset = tagName "feOffset" feOffsetAttrs $
   \(ca,pa,fpa,class_,style,in1,dx,dy) ->
   do return $ Leaf (id1 ca) Nothing mempty mempty

parseFeSpecularLighting :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeSpecularLighting = tagName "feSpecularLighting" feSpecularLightingAttrs $
   \(ca,pa,fpa,class_,style,in1,surfaceScale,sc,se,ku) ->
   do return $ Leaf (id1 ca) Nothing mempty mempty

parseFeTile :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeTile = tagName "feTile" feTileAttrs $
   \(ca,pa,fpa,class_,style,in1) ->
   do return $ Leaf (id1 ca) Nothing mempty mempty

parseFeTurbulence :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeTurbulence = tagName "feTurbulence" feTurbulenceAttrs $
   \(ca,pa,fpa,class_,style,in1,in2,mode) ->
   do return $ Leaf (id1 ca) Nothing mempty mempty

------------------------------------------------------------------------------------

animationElements = []

