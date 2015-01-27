{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExistentialQuantification #-}

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
import qualified Data.Colour
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
import Diagrams.SVG.Attributes 
import Diagrams.SVG.Path (commands, commandsToTrails, PathCommand(..))
import Diagrams.SVG.Tree 
import Filesystem.Path (FilePath)
import Text.XML.Stream.Parse hiding (parseText)
import Text.CSS.Parse (parseBlocks)
import Prelude hiding (FilePath)
import Data.Tuple.Select
import qualified Data.Conduit.List as CL

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
  do tree <- runResourceT $ parseFile def fp $$ force "error in parseSVG" parseSVG
              -- (C.map stripNamespace parseSVG)
     let (ns,css,grad) = nodes ([],[],[]) tree
     let nmap    = H.fromList ns -- needed because of the use-tag and clipPath
     let cssmap  = H.fromList css -- CSS inside the <defs> tag
     let gradmap = H.fromList $ map (\(id1,f) -> (id1, f cssmap)) grad
     let diagram = (insertRefs (nmap,cssmap,gradmap) tree) # scaleY (-1) -- # initialStyles -- insert references from hashmaps into tree
     return (preserveAspectRatio width height (Diagrams.TwoD.Size.width diagram) (Diagrams.TwoD.Size.height diagram) preserveAR diagram)

type Width = Double
type Height = Double

-- | According to <http://www.w3.org/TR/SVG11/coords.html#PreserveAspectRatioAttribute>
-- To Do: implement slice
preserveAspectRatio :: Width -> Height -> Width -> Height -> PreserveAR -> Diagram B R2 -> Diagram B R2
preserveAspectRatio newWidth newHeight oldWidth oldHeight (PAR alignXY Meet) image
   | aspectRatio < newAspectRatio = xPlace alignXY image
   | otherwise                    = yPlace alignXY image
  where aspectRatio = oldWidth / oldHeight
        newAspectRatio = newWidth / newHeight
        scaX = newHeight / oldHeight
        scaY = newWidth / oldWidth
        xPlace (AlignXY x y) i = i # scale scaX # alignBL # translateX ((newWidth  - oldWidth*scaX)*x)
        yPlace (AlignXY x y) i = i # scale scaY # alignBL # translateY ((newHeight - oldHeight*scaY)*y)

------------------------------------------------------------------------------------------------------------

-- | Lookup a diagram and return an empty diagram in case the SVG-file has a wrong reference
lookUp hmap i | isJust l  = fromJust l
              | otherwise = Leaf Nothing mempty mempty -- an empty diagram if we can't find the id
  where l = H.lookup i hmap

-- | Evaluate the tree into a diagram by inserting references and applying clipping
insertRefs :: (H.HashMap Text Tag, H.HashMap Text Attrs, H.HashMap Text Texture) -> Tag -> Diagram B R2
insertRefs maps (Leaf id1 path f) = f maps
insertRefs maps (Grad _ _) = mempty
insertRefs maps (Stop f)   = mempty
insertRefs maps (Reference selfId id1 (w,h) styles)
    | (isJust w && (fromJust w) <= 0) || (isJust h && (fromJust h) <= 0) = mempty
    | otherwise = referencedDiagram # styles maps
                                 -- # stretchViewBox (fromJust w) (fromJust h) viewboxPAR
                                 -- # cutOutViewBox viewboxPAR  
  where viewboxPAR = getViewboxPreserveAR subTree
        referencedDiagram = insertRefs maps (makeSubTreeVisible subTree)
        subTree = lookUp (sel1 maps) (Diagrams.SVG.Attributes.fragment id1) :: Tag
        getViewboxPreserveAR (SubTree _ id1 viewbox ar g children) = (viewbox, ar)
        getViewboxPreserveAR _ = (Nothing, Nothing)

insertRefs maps (SubTree True id1 viewbox ar styles children) =
    subdiagram # styles maps
             --  # stretchViewBox (Diagrams.TwoD.Size.width subdiagram) (Diagrams.TwoD.Size.height subdiagram) (viewbox, ar)
               # cutOutViewBox (viewbox, ar)
  where subdiagram = mconcat (map (insertRefs maps) children)

insertRefs maps (SubTree False _ _ _ _ _) = mempty

makeSubTreeVisible (SubTree _    id1 viewbox ar g children) =
                   (SubTree True id1 viewbox ar g (map makeSubTreeVisible children))
makeSubTreeVisible x = x

fragment x = fromMaybe T.empty $ fmap snd (parseTempl parseIRI x) -- look only for the text after "#"

stretchViewBox w h ((Just (minX,minY,width,height), Just par)) = preserveAspectRatio w h width height par
stretchViewBox w h ((Just (minX,minY,width,height), Nothing))  =
                                    preserveAspectRatio w h width height (PAR (AlignXY 0.5 0.5) Meet)
stretchViewBox w h _ = id

cutOutViewBox (Just (minX,minY,width,height), _) = (view (p2 (minX, minY)) (r2 ((width - minX), (height - minY))) )
                                                 --  (clipBy (rect (width - minX) (height - minY)))
cutOutViewBox _ = id

-------------------------------------------------------------------------------------
-- Basic SVG structure

-- | Parse \<svg\>, see <http://www.w3.org/TR/SVG/struct.html#SVGElement>
parseSVG :: MonadThrow m => Sink Event m (Maybe Tag)
parseSVG = tagName "{http://www.w3.org/2000/svg}svg" svgAttrs $
   \(cpa,ca,gea,pa,class_,style,ext,x,y,w,h,vb,ar,zp,ver,baseprof,cScripT,cStyleT,xmlns,xml) ->
   do gs <- many svgContent
      let st hmaps = (parseStyles style hmaps) ++ -- parse the style attribute (style="stop-color:#000000;stop-opacity:0.8")
                     (parsePA  pa  hmaps) ++ -- presentation attributes: stop-color="#000000" stop-opacity="0.8"
                     (cssStylesFromMap hmaps "svg" (id1 ca) class_)
      return $ SubTree True (id1 ca)
                            (parseViewBox vb)
                            (parsePreserveAR ar)
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

gContent = choose -- the likely most common are checked first
     [parsePath, parseRect, parseCircle, parseEllipse, parseLine, parsePolyLine, parsePolygon, -- shape elements
      parseG, parseDefs, parseStyle, parseSymbol, parseUse, -- structural elements
      parseLinearGradient, parseRadialGradient, parseClipPath, parseFilter, parseImage, parseText, 
      parsePattern, parseSwitch, parsePerspective,
      parseDesc, parseMetaData, parseTitle, parsePathEffect] -- descriptive Elements

---------------------------------------------------------------------------
-- | Parse \<defs\>, see <http://www.w3.org/TR/SVG/struct.html#DefsElement>
parseDefs :: MonadThrow m => Consumer Event m (Maybe Tag)
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

parseStyle :: MonadThrow m => Consumer Event m (Maybe Tag)
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
parseSymbol :: MonadThrow m => Consumer Event m (Maybe Tag)
parseSymbol = tagName "{http://www.w3.org/2000/svg}symbol" symbolAttrs $
   \(ca,gea,pa,class_,style,ext,ar,viewbox) ->
   do insideSym <- many gContent
      let st hmaps = (parseStyles style hmaps) ++
                     (parsePA  pa  hmaps) ++
                     (cssStylesFromMap hmaps "symbol" (id1 ca) class_)
      return $ SubTree False (id1 ca)
                             (parseViewBox viewbox)
                             (parsePreserveAR ar)
                             (applyStyleSVG st)
                             (reverse insideSym)

-----------------------------------------------------------------------------------
-- | Parse \<use\>, see <http://www.w3.org/TR/SVG/struct.html#UseElement>
parseUse = tagName "{http://www.w3.org/2000/svg}use" useAttrs
   $ \(ca,cpa,gea,pa,xlink,class_,style,ext,tr,x,y,w,h) ->
   do insideUse <- many useContent
      let st hmaps = (parseStyles style hmaps) ++
                     (parsePA  pa  hmaps) ++
                     (cssStylesFromMap hmaps "use" (id1 ca) class_)
      return $ Reference (id1 ca) (xlinkHref xlink) (parseToDouble w, parseToDouble h)
                         (\maps -> (translate (r2 (p x, p y))) . (applyTr (parseTr tr)) . (applyStyleSVG st maps))

useContent = choose [parseDesc,parseTitle] -- descriptive elements

--------------------------------------------------------------------------------------
-- | Parse \<switch\>, see <http://www.w3.org/TR/SVG/struct.html#SwitchElement>
parseSwitch = tagName "{http://www.w3.org/2000/svg}switch" switchAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr) ->
   do insideSwitch <- many switchContent
      return $ Leaf (id1 ca) mempty mempty

switchContent = choose [parsePath, parseRect, parseCircle, parseEllipse, parseLine, parsePolyLine, parsePolygon]

-----------------------------------------------------------------------------------
-- | Parse \<rect\>,  see <http://www.w3.org/TR/SVG11/shapes.html#RectElement>
parseRect :: MonadThrow m => Consumer Event m (Maybe Tag)
parseRect = tagName "{http://www.w3.org/2000/svg}rect" rectAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,ar,tr,x,y,w,h,rx,ry) -> do
    let st hmaps = (parseStyles style hmaps) ++
                   (parsePA  pa  hmaps) ++
                   (cssStylesFromMap hmaps "rect" (id1 ca) class_)
    return $ Leaf (id1 ca)
                  (path x y w h rx ry tr)
                  (\maps -> (path x y w h rx ry tr # stroke # applyStyleSVG st maps))

  where path x y w h rx ry tr = (rRect (p w) (p h) (p rx) (p ry)) # alignBL 
                                                                  # applyTr (parseTr tr)
                                                                  # translate (r2 (p x, p y))
        rRect pw ph prx pry | prx == 0 && pry == 0 = rect pw ph :: Path R2
                            | otherwise = roundedRect pw ph (if prx == 0 then pry else prx) :: Path R2

---------------------------------------------------------------------------------------------------
-- | Parse \<circle\>,  see <http://www.w3.org/TR/SVG11/shapes.html#CircleElement>
parseCircle :: MonadThrow m => Consumer Event m (Maybe Tag)
parseCircle = tagName "{http://www.w3.org/2000/svg}circle" circleAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,r,cx,cy) -> do
    let st hmaps = (parseStyles style hmaps) ++
                   (parsePA  pa  hmaps) ++
                   (cssStylesFromMap hmaps "circle" (id1 ca) class_)
    return $ Leaf (id1 ca)
                  (path cx cy r tr)
                  (\maps -> (path cx cy r tr # stroke # applyStyleSVG st maps))

  where path cx cy r tr = circle (p r) # applyTr (parseTr tr) # translate (r2 (p cx, p cy))

---------------------------------------------------------------------------------------------------
-- | Parse \<ellipse\>,  see <http://www.w3.org/TR/SVG11/shapes.html#EllipseElement>
parseEllipse :: MonadThrow m => Consumer Event m (Maybe Tag)
parseEllipse = tagName "{http://www.w3.org/2000/svg}ellipse" ellipseAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,rx,ry,cx,cy) -> do
    let st hmaps = (parseStyles style hmaps) ++
                   (parsePA  pa  hmaps) ++
                   (cssStylesFromMap hmaps "ellipse" (id1 ca) class_)
    return $ Leaf (id1 ca)
                  (path cx cy rx ry tr)
                  (\maps -> (path cx cy rx ry tr # stroke # applyStyleSVG st maps))

  where path cx cy rx ry tr = ellipseXY (p rx) (p ry) # applyTr (parseTr tr) # translate (r2 (p cx, p cy))

---------------------------------------------------------------------------------------------------
-- | Parse \<line\>,  see <http://www.w3.org/TR/SVG11/shapes.html#LineElement>
parseLine :: MonadThrow m => Consumer Event m (Maybe Tag)
parseLine = tagName "{http://www.w3.org/2000/svg}line" lineAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,x1,y1,x2,y2) -> do
    let st hmaps = (parseStyles style hmaps) ++
                   (parsePA  pa  hmaps) ++
                   (cssStylesFromMap hmaps "line" (id1 ca) class_)
    return $ Leaf (id1 ca)
                  (path x1 y1 x2 y2 tr)
                  (\maps -> (path x1 y1 x2 y2 tr # stroke # applyStyleSVG st maps))

  where path x1 y1 x2 y2 tr =
               fromSegments [ straight (r2 ((p x2) - (p x1), (p y2) - (p y1))) ]
               # applyTr (parseTr tr)
               # translate (r2 (p x1, p y1))

---------------------------------------------------------------------------------------------------
-- | Parse \<polyline\>,  see <http://www.w3.org/TR/SVG11/shapes.html#PolylineElement>
parsePolyLine :: MonadThrow m => Consumer Event m (Maybe Tag)
parsePolyLine = tagName "{http://www.w3.org/2000/svg}polyline" polygonAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,points) -> do
    let st hmaps = (parseStyles style hmaps) ++
                   (parsePA  pa  hmaps) ++
                   (cssStylesFromMap hmaps "polyline" (id1 ca) class_)
    let ps = parsePoints (fromJust points)
    return $ Leaf (id1 ca)
                  (path ps tr)
                  (\maps -> (dia  ps tr # applyStyleSVG st maps))

  where path ps tr = fromVertices (map p2 ps) # applyTr (parseTr tr)
                                              # translate (r2 (head ps))

        dia  ps tr = fromVertices (map p2 ps) # strokeLine
                                              # applyTr (parseTr tr)
                                              # translate (r2 (head ps))

--------------------------------------------------------------------------------------------------
-- | Parse \<polygon\>,  see <http://www.w3.org/TR/SVG11/shapes.html#PolygonElement>
parsePolygon :: MonadThrow m => Consumer Event m (Maybe Tag)
parsePolygon = tagName "{http://www.w3.org/2000/svg}polygon" polygonAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,points) -> do
    let st hmaps = (parseStyles style hmaps) ++
                   (parsePA  pa  hmaps) ++
                   (cssStylesFromMap hmaps "polygon" (id1 ca) class_)
    let ps = parsePoints (fromJust points)
    return $ Leaf (id1 ca)
                  (path  ps tr)
                  (\maps -> (dia   ps tr # applyStyleSVG st maps))

  where path ps tr = fromVertices (map p2 ps) # applyTr (parseTr tr)
                                              # translate (r2 (head ps))

        dia  ps tr = fromVertices (map p2 ps) # closeLine
                                              # strokeLoop
                                              # applyTr (parseTr tr)
                                              # translate (r2 (head ps))

--------------------------------------------------------------------------------------------------
-- | Parse \<path\>,  see <http://www.w3.org/TR/SVG11/paths.html#PathElement>
parsePath :: MonadThrow m => Consumer Event m (Maybe Tag)
parsePath = tagName "{http://www.w3.org/2000/svg}path" pathAttrs $
  \(cpa,ca,gea,pa,class_,style,ext,tr,d,pathLength) -> do
    let st hmaps = (parseStyles style hmaps) ++
                   (parsePA  pa  hmaps) ++
                   (cssStylesFromMap hmaps "path" (id1 ca) class_)
    return $ Leaf (id1 ca)
                  (path d tr)
                  (\maps -> (path d tr # stroke # applyStyleSVG st maps))

  where path d tr = (mconcat $ commandsToTrails $ commands d) # applyTr (parseTr tr)

-------------------------------------------------------------------------------------------------
-- | Parse \<clipPath\>, see <http://www.w3.org/TR/SVG/masking.html#ClipPathElement>
parseClipPath :: MonadThrow m => Consumer Event m (Maybe Tag)
parseClipPath = tagName "{http://www.w3.org/2000/svg}clipPath" clipPathAttrs $
  \(cpa,ca,pa,class_,style,ext,ar,viewbox) -> do
    insideClipPath <- many clipPathContent
    let st hmaps = (parseStyles style hmaps) ++
                   (parsePA  pa  hmaps) ++
                   (cssStylesFromMap hmaps "clipPath" (id1 ca) class_)
    return $ SubTree False (id1 ca) (parseViewBox viewbox) (parsePreserveAR ar) (applyStyleSVG st) (reverse insideClipPath)

clipPathContent = choose [parsePath, parseRect, parseCircle, parseEllipse, parseLine, parsePolyLine, parsePolygon,
                          parseText, parseUse]

--------------------------------------------------------------------------------------
-- | Parse \<image\>, see <http://www.w3.org/TR/SVG/struct.html#ImageElement>
parseImage :: MonadThrow m => Consumer Event m (Maybe Tag)
parseImage = tagName "{http://www.w3.org/2000/svg}image" imageAttrs $
  \(ca,cpa,gea,xlink,pa,class_,style,ext,ar,tr,x,y,w,h) ->
  do return $ Leaf (id1 ca) mempty mempty

-- | Parse \<text\>, see <http://www.w3.org/TR/SVG/text.html#TextElement>
parseText :: MonadThrow m => Consumer Event m (Maybe Tag)
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
parseLinearGradient :: MonadThrow m => Consumer Event m (Maybe Tag)
parseLinearGradient = tagName "{http://www.w3.org/2000/svg}linearGradient" linearGradAttrs $
  \(cpa,ca,pa,xlink,class_,style,ext,x1,y1,x2,y2,gradientUnits,gradientTransform,spreadMethod) ->
  do gs <- many gradientContent
     let stops :: [CSSMap -> [GradientStop]]
         stops = map getTexture $ concat $ map extractStops gs
     -- stops are lists of functions and everyone of these gets passed the same cssmap
     -- and puts them into a Grad constructor
     return $ Grad (id1 ca) (\css -> (mkLinearGradient (concat (map ($ css) stops))
                                                       ((parseMaybeDouble x1) ^& (parseMaybeDouble y1))
                                                       ((parseMaybeDouble x2) ^& (parseMaybeDouble y2)) GradPad)  )

gradientContent = choose
     [parseStop, parseMidPointStop, parseSet,
      parseDesc, parseMetaData, parseTitle] -- descriptive Elements (rarely used here, so tested at the end)

-- | Parse \<radialGradient\>, see <http://www.w3.org/TR/SVG/pservers.html#RadialGradientElement>
parseRadialGradient :: MonadThrow m => Consumer Event m (Maybe Tag)
parseRadialGradient = tagName "{http://www.w3.org/2000/svg}radialGradient" radialGradAttrs $
  \(cpa,ca,pa,xlink,class_,style,ext,cx,cy,r,fx,fy,gradientUnits,gradientTransform,spreadMethod) ->
  do gs <- many gradientContent
     let stops :: [CSSMap -> [GradientStop]]
         stops = map getTexture $ concat $ map extractStops gs
     return $ Grad (id1 ca) (\css -> (mkRadialGradient (concat (map ($ css) stops))
                                                             ((parseMaybeDouble cx) ^& (parseMaybeDouble cy))
                                                             (parseMaybeDouble r)
                                                             (0 ^& 0) 0 GradPad)  )

extractStops :: Tag -> [Tag]
extractStops (SubTree b id1 viewBox ar f children) = concat (map extractStops children)
extractStops (Stop stops) = [Stop stops]
extractStops _ = []

getTexture :: Tag -> (CSSMap -> [GradientStop])
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
      return $ Stop (\hmaps -> mkStops [getStopTriple (parseMaybeDouble offset) (st hmaps)])

parseMidPointStop = tagName "{http://www.w3.org/2000/svg}midPointStop" stopAttrs $
   \(ca,pa,xlink,class_,style,offset) ->
   do let st hmaps = (parseStyles style empty3) ++
                     (parsePA pa empty3) ++
                     (cssStylesFromMap hmaps "midPointStop" (id1 ca) class_)
      return $ Stop (\hmaps -> mkStops [getStopTriple (parseMaybeDouble offset) (st hmaps)])

empty3 = (H.empty,H.empty,H.empty)
	  
-- (An opaque color, a stop fraction, an opacity).
-- mkStops :: [(Colour Double, Double, Double)] -> [GradientStop]
-- mkStops [(gray, 0, 1), (white, 0.5, 1), (purple, 1, 1)]

getStopTriple offset styles = -- Debug.Trace.trace (show styles ++ show (col c, offset, opacity o))
                              (col c, offset, opacity o)
  where col [Fill c] = blue -- TODO extract colour channel
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

----------------------------------------------------------------------------------------
-- descriptive elements
------------------------------------------------------o	----------------------------------
-- | Parse \<desc\>, see <http://www.w3.org/TR/SVG/struct.html#DescriptionAndTitleElements>
parseDesc :: MonadThrow m => Consumer Event m (Maybe Tag)
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
parseMetaData = tagName "{http://www.w3.org/2000/svg}metadata" ignoreAttrs
   $ \_ ->
   do meta <- many metaContent
      return $ Leaf Nothing mempty mempty

metaContent = choose [parseRDF] -- extend if needed

parseRDF = tagName "{http://www.w3.org/1999/02/22-rdf-syntax-ns#}RDF" ignoreAttrs
          $ \_ ->
          do c <- parseWork
             return $ Leaf Nothing mempty mempty

parseWork = tagName "{http://creativecommons.org/ns#}Work" ignoreAttrs
   $ \_ ->
   do c <- many workContent
      return $ Leaf Nothing mempty mempty

workContent = choose [parseFormat, parseType, parseRDFTitle, parseDate, parseCreator,
                      parsePublisher, parseSource, parseLanguage, parseSubject, parseDescription]

parseFormat = tagName "{http://purl.org/dc/elements/1.1/}format" ignoreAttrs
   $ \_ -> do { c <- content ; return $ Leaf Nothing mempty mempty }

parseType = tagName "{http://purl.org/dc/elements/1.1/}type" ignoreAttrs
   $ \_ -> do { c <- content ; return $ Leaf Nothing mempty mempty }

parseRDFTitle = tagName "{http://purl.org/dc/elements/1.1/}title" ignoreAttrs
   $ \_ -> do { c <- content ; return $ Leaf Nothing mempty mempty }

parseDate = tagName "{http://purl.org/dc/elements/1.1/}date" ignoreAttrs
   $ \_ -> do { c <- content ; return $ Leaf Nothing mempty mempty }

parseCreator = tagName "{http://purl.org/dc/elements/1.1/}creator" ignoreAttrs
   $ \_ -> do { c <- parseAgent ; return $ Leaf Nothing mempty mempty }

parseAgent = tagName "{http://creativecommons.org/ns#}Agent" ignoreAttrs
   $ \_ -> do { c <- parseAgentTitle ; return $ Leaf Nothing mempty mempty }

parseAgentTitle = tagName "{http://purl.org/dc/elements/1.1/}title" ignoreAttrs
   $ \_ -> do { c <- content ; return $ Leaf Nothing mempty mempty }

parsePublisher = tagName "{http://purl.org/dc/elements/1.1/}publisher" ignoreAttrs
   $ \_ -> do { c <- parseAgent ; return $ Leaf Nothing mempty mempty }

parseSource = tagName "{http://purl.org/dc/elements/1.1/}source" ignoreAttrs
   $ \_ -> do { c <- content ; return $ Leaf Nothing mempty mempty }

parseLanguage = tagName "{http://purl.org/dc/elements/1.1/}language" ignoreAttrs
   $ \_ -> do { c <- content ; return $ Leaf Nothing mempty mempty }

parseSubject = tagName "{http://purl.org/dc/elements/1.1/}subject" ignoreAttrs
   $ \_ -> do { c <- parseBag ; return $ Leaf Nothing mempty mempty }

parseBag = tagName "{http://www.w3.org/1999/02/22-rdf-syntax-ns#}Bag" ignoreAttrs
   $ \_ -> do { c <- parseList ; return $ Leaf Nothing mempty mempty }

parseList = tagName "{http://www.w3.org/1999/02/22-rdf-syntax-ns#}li" ignoreAttrs
   $ \_ -> do { c <- content ; return $ Leaf Nothing mempty mempty }

parseDescription = tagName "{http://purl.org/dc/elements/1.1/}description" ignoreAttrs
   $ \_ -> do { c <- content ; return $ Leaf Nothing mempty mempty }

test :: Data.XML.Types.Name
test = "{http://www.w3.org/1999/02/22-rdf-syntax-ns#}RDF"

------------------------------------
-- inkscape / sodipodi tags
------------------------------------

parseSodipodi = tagName "{http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd}namedview" namedViewAttrs
   $ \(pc,bc,bo,ot,gt,gut,po,ps,ww,wh,id1,sg,zoom,cx,cy,wx,wy,wm,cl) ->
   do c <- parseGrid
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
   $ \(typ,vp_x,vp_y,vp_z,persp3d_origin,id_) -> return $ Leaf (Just "") mempty mempty

parsePathEffect = tagName "{http://www.inkscape.org/namespaces/inkscape}path-effect" ignoreAttrs
   $ \_ -> return $ Leaf Nothing mempty mempty
--------------------------------------------------------------------------------------
-- sceletons

-- | Parse \<pattern\>, see <http://www.w3.org/TR/SVG/pservers.html#PatternElement>
parsePattern :: MonadThrow m => Consumer Event m (Maybe Tag)
parsePattern = tagName "{http://www.w3.org/2000/svg}pattern" patternAttrs $
  \(cpa,ca,pa,class_,style,ext,view,ar,x,y,w,h,pUnits,pCUnits,pTrans) ->
  do insideSym <- many patternContent
     return $ Leaf (id1 ca) mempty mempty

patternContent = choose [parseImage]

-- | Parse \<filter\>, see <http://www.w3.org/TR/SVG/filters.html#FilterElement>
parseFilter :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFilter = tagName "{http://www.w3.org/2000/svg}filter" filterAttrs $
  \(ca,pa,xlink,class_,style,ext,x,y,w,h,filterRes,filterUnits,primUnits) ->
  do insideSym <- many filterContent
     return $ Leaf (id1 ca) mempty mempty

filterContent = choose [ parseFeGaussianBlur,
    parseFeBlend,parseFeColorMatrix,parseFeComponentTransfer,parseFeComposite,parseFeConvolveMatrix, -- filter primitive elments
    parseFeDiffuseLighting,parseFeDisplacementMap,parseFeFlood,parseFeImage,
    parseFeMerge,parseFeMorphology,parseFeOffset,parseFeSpecularLighting,parseFeTile,parseFeTurbulence,parseDesc,parseTitle]

--------------------------------------------------------------------------------------
-- filter primitives (currently only sceletons)
--------------------------------------------------------------------------------------

-- | Parse \<feBlend\>, see <http://www.w3.org/TR/SVG/filters.html#feBlendElement>
parseFeBlend :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeBlend = tagName "{http://www.w3.org/2000/svg}feBlend" feBlendAttrs $
   \(ca,pa,fpa,class_,style,in1,in2,mode) ->
   do return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feColorMatrix\>, see <http://www.w3.org/TR/SVG/filters.html#feColorMatrixElement>
parseFeColorMatrix :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeColorMatrix = tagName "{http://www.w3.org/2000/svg}feColorMatrix" feColorMatrixAttrs $
   \(ca,pa,fpa,class_,style,in1,type1,values) ->
   do return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feComponentTransfer\>, see <http://www.w3.org/TR/SVG/filters.html#feComponentTransferElement>
parseFeComponentTransfer :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeComponentTransfer = tagName "{http://www.w3.org/2000/svg}feComponentTransfer" feComponentTransferAttrs $
   \(ca,pa,fpa,class_,style,in1) ->
   do return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feComposite\>, see <http://www.w3.org/TR/SVG/filters.html#feCompositeElement>
parseFeComposite :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeComposite = tagName "{http://www.w3.org/2000/svg}feComposite" feCompositeAttrs $
   \(ca,pa,fpa,class_,style,in1,in2,operator,k1,k2,k3,k4) ->
   do return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feConvolveMatrix\>, see <http://www.w3.org/TR/SVG/filters.html#feConvolveMatrixElement>
parseFeConvolveMatrix :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeConvolveMatrix = tagName "{http://www.w3.org/2000/svg}feConvolveMatrix" feConvolveMatrixAttrs $
   \(ca,pa,fpa,class_,style,order,km,d,bias,tx,ty,em,ku,par) ->
   do return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feDiffuseLighting\>, see <http://www.w3.org/TR/SVG/filters.html#feDiffuseLightingElement>
parseFeDiffuseLighting :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeDiffuseLighting = tagName "{http://www.w3.org/2000/svg}feDiffuseLighting" feDiffuseLightingAttrs $
   \(ca,pa,fpa,class_,style,in1,surfaceScale,diffuseConstant,kuLength) ->
   do return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feDisplacementMap\>, see <http://www.w3.org/TR/SVG/filters.html#feDisplacementMapElement>
parseFeDisplacementMap :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeDisplacementMap = tagName "{http://www.w3.org/2000/svg}feDisplacementMap" feDisplacementMapAttrs $
   \(ca,pa,fpa,class_,style,in1,in2,sc,xChan,yChan) ->
   do return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feFlood\>, see <http://www.w3.org/TR/SVG/filters.html#feFloodElement>
parseFeFlood :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeFlood = tagName "{http://www.w3.org/2000/svg}feFlood" feFloodAttrs $
   \(ca,pa,fpa,class_,style) ->
   do return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feGaussianBlur\>, see <http://www.w3.org/TR/SVG/filters.html#feGaussianBlurElement>
parseFeGaussianBlur :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeGaussianBlur = tagName "{http://www.w3.org/2000/svg}feGaussianBlur" feGaussianBlurAttrs $
   \(ca,pa,fpa,class_,style,in1,stdDeviation) ->
   do return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feImage\>, see <http://www.w3.org/TR/SVG/filters.html#feImageElement>
parseFeImage :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeImage = tagName "{http://www.w3.org/2000/svg}feImage" feImageAttrs $
   \(ca,pa,fpa,xlibk,class_,style,ext,par) ->
   do return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feMerge\>, see <http://www.w3.org/TR/SVG/filters.html#feMergeElement>
parseFeMerge :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeMerge = tagName "{http://www.w3.org/2000/svg}feMerge" feMergeAttrs $
   \(ca,pa,fpa,class_,style) ->
   do return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feMorphology\>, see <http://www.w3.org/TR/SVG/filters.html#feMorphologyElement>
parseFeMorphology :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeMorphology = tagName "{http://www.w3.org/2000/svg}feMorphology" feMorphologyAttrs $
   \(ca,pa,fpa,class_,style,in1,operator,radius) ->
   do return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feOffset\>, see <http://www.w3.org/TR/SVG/filters.html#feOffsetElement>
parseFeOffset :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeOffset = tagName "{http://www.w3.org/2000/svg}feOffset" feOffsetAttrs $
   \(ca,pa,fpa,class_,style,in1,dx,dy) ->
   do return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feSpecularLighting\>, see <http://www.w3.org/TR/SVG/filters.html#feSpecularLightingElement>
parseFeSpecularLighting :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeSpecularLighting = tagName "{http://www.w3.org/2000/svg}feSpecularLighting" feSpecularLightingAttrs $
   \(ca,pa,fpa,class_,style,in1,surfaceScale,sc,se,ku) ->
   do return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feTile\>, see <http://www.w3.org/TR/SVG/filters.html#feTileElement>
parseFeTile :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeTile = tagName "{http://www.w3.org/2000/svg}feTile" feTileAttrs $
   \(ca,pa,fpa,class_,style,in1) ->
   do return $ Leaf (id1 ca) mempty mempty

-- | Parse \<feTurbulence\>, see <http://www.w3.org/TR/SVG/filters.html#feTurbulenceElement>
parseFeTurbulence :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeTurbulence = tagName "{http://www.w3.org/2000/svg}feTurbulence" feTurbulenceAttrs $
   \(ca,pa,fpa,class_,style,in1,in2,mode) ->
   do return $ Leaf (id1 ca) mempty mempty

------------------------------------------------------------------------------------

animationElements = []

