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
    -- * Tree Structure
    , Tag(..)
    , Id(..)
    , ClipRef(..)
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
    -- * Parsing of other tags
    , parseClipPath
    , parsePattern
    , parseFilter
    , parseImage
    , parseText
    -- * Attribute Parsing of classes of attributes
    , coreAttributes
    , conditionalProcessingAttributes
    , documentEventAttributes
    , graphicalEventAttributes
    , presentationAttributes
    , filterPrimitiveAttributes
    , xlinkAttributes
    , xmlnsNameSpaces
    -- * Attributes for basic structure elements
    , svgAttrs
    , gAttrs
    , descAttrs
    , symbolAttrs
    , useAttrs
    , switchAttrs
    -- * Attributes for basic shape elements
    , rectAttrs
    , circleAttrs
    , ellipseAttrs
    , lineAttrs
    , polygonAttrs
    , pathAttrs
    -- * Other Attributes
    , clipPathAttrs
    , patternAttrs
    , imageAttrs
    , filterAttrs
    , textAttrs
    , namedViewAttrs
    -- * Filter Effect Attributes
    , feBlendAttrs
    , feColorMatrixAttrs
    , feComponentTransferAttrs
    , feCompositeAttrs
    , feConvolveMatrixAttrs
    , feDiffuseLightingAttrs
    , feDisplacementMapAttrs
    , feFloodAttrs
    , feGaussianBlurAttrs
    , feImageAttrs
    , feMergeAttrs
    , feMorphologyAttrs
    , feOffsetAttrs
    , feSpecularLightingAttrs
    , feTileAttrs
    , feTurbulenceAttrs
    ) where

import Data.Conduit
--import qualified Data.Conduit.List as C
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust, catMaybes, fromMaybe, isJust)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.XML.Types
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.Ellipse
import Diagrams.TwoD.Size
import Diagrams.TwoD.Types
import Diagrams.SVG.Attributes (applyTr, parseTr, applyStyleSVG, parseStyles, parseDouble, parsePoints, parsePA, CoreAttributes(..), ConditionalProcessingAttributes(..), DocumentEventAttributes(..), GraphicalEventAttributes(..), PresentationAttributes(..), XlinkAttributes(..), FilterPrimitiveAttributes(..), NameSpaces(..), PreserveAR(..), AlignSVG(..), Place(..), MeetOrSlice(..), p, fragment)
import Diagrams.SVG.Path (commands, commandsToTrails, PathCommand(..))
import Prelude hiding (FilePath)
import Filesystem.Path (FilePath)
import Text.XML.Stream.Parse hiding (parseText)
import Debug.Trace


coreAttributes =
  do l <- mapM optionalAttr
      [ "id", "base", "lang", "space"] -- "xml:base", "xml:lang", "xml:space"]
     return $ (\[a,b,c,d] -> CA a b c d) l

conditionalProcessingAttributes =
  do l <- mapM optionalAttr
      [ "requiredFeatures", "requiredExtensions", "systemLanguage"]
     return $ (\[a,b,c] -> CPA a b c) l

documentEventAttributes =
  do l <- mapM optionalAttr
      [ "onunload", "onabort", "onerror", "onresize", "onscroll", "onzoom"]
     return $ (\[a,b,c,d,e,f] -> DEA a b c d e f) l

graphicalEventAttributes =
  do l <- mapM optionalAttr
      [ "onfocusin", "onfocusout", "onactivate", "onclick", "onmousedown", "onmouseup", 
        "onmouseover", "onmousemove", "onmouseout", "onload"]
     return $ (\[a,b,c,d,e,f,g,h,i,j] -> GEA a b c d e f g h i j) l

presentationAttributes =
  do l <- mapM optionalAttr
      ["alignmentBaseline","baseline-shift","clip","clip-path", "clip-rule",
       "color", "color-interpolation", "color-interpolation-filters", "color-profile",
       "color-rendering", "cursor", "direction", "display", "dominant-baseline", "enable-background",
       "fill", "fill-opacity", "fill-rule", "filter", "flood-color", "flood-opacity", "font-family",
       "font-size", "font-size-adjust", "font-stretch", "font-style", "font-variant", "font-weight",
       "glyph-orientation-horizontal", "glyph-orientation-vertical", "image-rendering", "kerning",
       "letter-spacing", "lighting-color", "marker-end", "marker-mid", "marker-start", "mask",
       "opacity", "overflow", "pointer-events", "shape-rendering", "stop-color", "stop-opacity",
       "stroke", "stroke-dasharray", "stroke-dashoffset", "stroke-linecap", "stroke-linejoin",
       "stroke-miterlimit", "stroke-opacity", "stroke-width", "text-anchor", "text-decoration",
       "text-rendering", "unicode-bidi", "visibility", "word-spacing", "writing-mode"]
     return $
      (\[a,b,c0,c1,c2,c3,c4,c5,c6,c7,c8,d0,d1,d2,e,f0,f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,
        g0,g1,i,k,l0,l1,m0,m1,m2,m3,o0,o1,p,s0,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,t0,t1,t2,u,v,w0,w1] ->
        PA a b c0 c1 c2 c3 c4 c5 c6 c7 c8 d0 d1 d2 e f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12
        g0 g1 i k l0 l1 m0 m1 m2 m3 o0 o1 p s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 t0 t1 t2 u v w0 w1 ) l

filterPrimitiveAttributes =
  do l <- mapM optionalAttr
      [ "x","y","widh","height","result"]
     return $ (\[x,y,w,h,r] -> FPA x y w h r) l


-- prefix :: Maybe T.Text -> T.Text -> Data.XML.Types.Name
-- prefix ns attribute = Name attribute ns Nothing

xlinkAttributes = -- xlinkNamespace is usually http://www.w3.org/1999/xlink
  do l <- mapM optionalAttr
      [ "{http://www.w3.org/1999/xlink}href", "{http://www.w3.org/1999/xlink}show", "{http://www.w3.org/1999/xlink}actuate",
        "{http://www.w3.org/1999/xlink}type", "{http://www.w3.org/1999/xlink}role", "{http://www.w3.org/1999/xlink}arcrole",
        "{http://www.w3.org/1999/xlink}title"]
     return $ (\[a,b,c,d,e,f,g] -> XLA a b c d e f g) l

xmlnsNameSpaces =
  do l <- mapM optionalAttr
      [ "{http://www.w3.org/2000/svg}xlink","{http://www.w3.org/2000/svg}dc", "{http://www.w3.org/2000/svg}cc",
        "{http://www.w3.org/2000/svg}rdf", "{http://www.w3.org/2000/svg}svg", "{http://www.w3.org/2000/svg}sodipodi",
        "{http://www.w3.org/2000/svg}inkscape" ]
     return $ (\[xlink,dc,cc,rdf,svg,sodipodi,inkscape] -> NSP xlink dc cc rdf svg sodipodi inkscape) l

xmlNameSpaces =
  do l <- mapM optionalAttr
      [ "{http://www.w3.org/XML/1998/namespace}space" ] -- the only attribute that seems to be used so far in the xml namespace is  xml:space="preserve"
     return $ (\[space] -> space) l

--------------------------------------------------------------------------------------
-- Attributes for basic structure tags, see http://www.w3.org/TR/SVG/struct.html
--------------------------------------------------------------------------------------

-- | Attributes for \<svg\>, see <http://www.w3.org/TR/SVG/struct.html#SVGElement>
svgAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     xmlns <- xmlnsNameSpaces
     xml <- xmlNameSpaces
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","x","y","width","height","viewBox","preserveAspectRatio",
       "zoomAndPan", "version", "baseProfile", "contentScriptType", "contentStyleType"]
     ignoreAttrs
     return $ (\[class_,style,ext,x,y,w,h,view,ar,zp,ver,baseprof,cScripT,cStyleT] -> 
              (cpa,ca,gea,pa,class_,style,ext,x,y,w,h,view,ar,zp,ver,baseprof,cScripT,cStyleT,xmlns,xml)) l

-- | Attributes for \<g\> and \<defs\>, see <http://www.w3.org/TR/SVG/struct.html#GElement>
gAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     class_ <- optionalAttr "class"
     style <- optionalAttr "style"
     ext <- optionalAttr "externalResourceRequired"
     tr <- optionalAttr "transform"
     ignoreAttrs
     return (cpa,ca,gea,pa,class_,style,ext,tr)

-- | Attributes for \<desc\>, see <http://www.w3.org/TR/SVG/struct.html#DescriptionAndTitleElements>
descAttrs =
  do ca <- coreAttributes
     class_ <- optionalAttr "class"
     style <- optionalAttr "style"
     ignoreAttrs
     return (ca,class_,style)	 

-- | Attributes for \<symbol\>, see <http://www.w3.org/TR/SVG/struct.html#SymbolElement>
symbolAttrs =
  do ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","preserveAspectRatio","viewBox"]
     ignoreAttrs
     return $ (\[class_,style,ext,ar,viewbox] -> 
               (ca,gea,pa,class_,style,ext,ar,viewbox) ) l

-- | Attributes for \<use\>, see <http://www.w3.org/TR/SVG/struct.html#UseElement>
useAttrs =
  do ca <- coreAttributes
     cpa <- conditionalProcessingAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     xlink <- xlinkAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","transform","x","y","width","height"]
     ignoreAttrs
     return $ (\[class_,style,ext,tr,x,y,w,h] -> 
      (ca,cpa,gea,pa,xlink,class_,style,ext,tr,x,y,w,h)) l

-- | Attributes for \<switch\>, see <http://www.w3.org/TR/SVG/struct.html#SwitchElement>
switchAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     class_ <- optionalAttr "class"
     style <- optionalAttr "style"
     ext <- optionalAttr "externalResourcesRequired"
     tr <- optionalAttr "transform"
     ignoreAttrs
     return (cpa,ca,gea,pa,class_,style,ext,tr)

--------------------------------------------------------------------------------------
-- Attributes for basic shape tags
--------------------------------------------------------------------------------------

-- | Attributes for \<rect\>,  see <http://www.w3.org/TR/SVG11/shapes.html#RectElement>
rectAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","preserveAspectRatio","transform","x","y",
       "width","height","rx","ry"]
     ignoreAttrs
     return $ (\[class_,style,ext,ar,tr,x,y,w,h,rx,ry] -> 
               (cpa,ca,gea,pa,class_,style,ext,ar,tr,x,y,w,h,rx,ry) ) l

-- | Attributes for \<circle\>,  see <http://www.w3.org/TR/SVG11/shapes.html#CircleElement>
circleAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","transform","r","cx","cy"]
     ignoreAttrs
     return $ (\[class_,style,ext,tr,r,cx,cy] -> 
               (cpa,ca,gea,pa,class_,style,ext,tr,r,cx,cy) ) l

-- | Attributes for \<ellipse\>,  see <http://www.w3.org/TR/SVG11/shapes.html#EllipseElement>
ellipseAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","transform","rx","ry","cx","cy"]
     ignoreAttrs
     return $ (\[class_,style,ext,tr,rx,ry,cx,cy] -> 
               (cpa,ca,gea,pa,class_,style,ext,tr,rx,ry,cx,cy) ) l

-- | Attributes for \<line\>,  see <http://www.w3.org/TR/SVG11/shapes.html#LineElement>
lineAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","transform","x1","y1","x2","y2"]
     ignoreAttrs
     return $ (\[class_,style,ext,tr,x1,y1,x2,y2] -> 
               (cpa,ca,gea,pa,class_,style,ext,tr,x1,y1,x2,y2) ) l

-- | Attributes for \<polygon\>,  see <http://www.w3.org/TR/SVG11/shapes.html#PolygonElement>
polygonAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","transform","points"]
     ignoreAttrs
     return $ (\[class_,style,ext,tr,points] -> 
               (cpa,ca,gea,pa,class_,style,ext,tr,points) ) l

-- | Attributes for \<path\>,  see <http://www.w3.org/TR/SVG11/paths.html#PathElement>
pathAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","transform","d","pathLength"]
     ignoreAttrs
     return $ (\[class_,style,ext,tr,d,pathLength] -> 
               (cpa,ca,gea,pa,class_,style,ext,tr,d,pathLength) ) l

-------------------------------------------------------------------------------------
-- | Attributes for \<clipPath\>, see <http://www.w3.org/TR/SVG/masking.html#ClipPathElement>
clipPathAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","transform","clipPathUnits"]
     ignoreAttrs
     return $ (\[class_,style,ext,tr,units] -> 
               (cpa,ca,pa,class_,style,ext,tr,units) ) l

-- | Attributes for \<pattern\>, see <http://www.w3.org/TR/SVG/pservers.html#PatternElement>
patternAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","viewBox","preserveAspectRatio","x","y",
       "width","height","patternUnits","patternContentUnits","patternTransform"]
     ignoreAttrs
     return $ (\[class_,style,ext,view,ar,x,y,w,h,pUnits,pCUnits,pTrans] -> 
               (cpa,ca,pa,class_,style,ext,view,ar,x,y,w,h,pUnits,pCUnits,pTrans) ) l

-- | Attributes for \<image\>, see <http://www.w3.org/TR/SVG/struct.html#ImageElement>
imageAttrs =
  do ca <- coreAttributes
     cpa <- conditionalProcessingAttributes
     gea <- graphicalEventAttributes
     xlink <- xlinkAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","preserveAspectRatio","transform",
       "x","y","width","height"]
     ignoreAttrs
     return $ (\[class_,style,ext,ar,tr,x,y,w,h] -> 
               (ca,cpa,gea,xlink,pa,class_,style,ext,ar,tr,x,y,w,h) ) l

-- | Attributes for \<filter\>, see <http://www.w3.org/TR/SVG/filters.html#FilterElement>
filterAttrs =
  do ca <- coreAttributes
     pa <- presentationAttributes
     xlink <- xlinkAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","x","y","width","height","filterRes","filterUnits","primitiveUnits"]
     ignoreAttrs
     return $ (\[class_,style,ext,x,y,w,h,filterRes,filterUnits,primUnits] -> 
                (ca,pa,xlink,class_,style,ext,x,y,w,h,filterRes,filterUnits,primUnits) ) l

-- | Attributes for \<text\>, see <http://www.w3.org/TR/SVG/text.html#TextElement>
textAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","transform","lengthAdjust",
       "x","y","dx","dy","rotate","textLength"]
     ignoreAttrs
     return $ (\[class_,style,ext,tr,la,x,y,dx,dy,rot,textlen] -> 
               (cpa,ca,gea,pa,class_,style,ext,tr,la,x,y,dx,dy,rot,textlen) ) l

namedViewAttrs =
  do 
     l <- mapM optionalAttr
      ["pagecolor","bordercolor","borderopacity","objecttolerance","gridtolerance",
       "guidetolerance", "id","showgrid"]
     inkscape <- mapM optionalAttr
       [ "{http://www.inkscape.org/namespaces/inkscape}pageopacity", "{http://www.inkscape.org/namespaces/inkscape}pageshadow",
         "{http://www.inkscape.org/namespaces/inkscape}window-width", "{http://www.inkscape.org/namespaces/inkscape}window-height",
         "{http://www.inkscape.org/namespaces/inkscape}zoom",
         "{http://www.inkscape.org/namespaces/inkscape}cx", "{http://www.inkscape.org/namespaces/inkscape}cy",
         "{http://www.inkscape.org/namespaces/inkscape}window-x", "{http://www.inkscape.org/namespaces/inkscape}window-y",
         "{http://www.inkscape.org/namespaces/inkscape}window-maximized", "{http://www.inkscape.org/namespaces/inkscape}current-layer"]
     ignoreAttrs
     return $ (\[pc,bc,bo,ot,gt,gut,id1,sg] [po,ps,ww,wh,zoom,cx,cy,wx,wy,wm,cl]->
                (pc,bc,bo,ot,gt,gut,po,ps,ww,wh,id1,sg,zoom,cx,cy,wx,wy,wm,cl) ) l inkscape

-------------------------------------------------------------------------------------------------------------

feBlendAttrs =
  do ca <- coreAttributes
     pa <- presentationAttributes
     fpa <- filterPrimitiveAttributes
     l <- mapM optionalAttr
      ["class","style","in","in2","mode"]
     return $ (\[class_,style,in1,in2,mode] -> (ca,pa,fpa,class_,style,in1,in2,mode) ) l

feColorMatrixAttrs =
  do ca <- coreAttributes
     pa <- presentationAttributes
     fpa <- filterPrimitiveAttributes
     l <- mapM optionalAttr
      ["class","style","in","type","values"]
     return $ (\[class_,style,in1,type1,values] -> (ca,pa,fpa,class_,style,in1,type1,values) ) l

feComponentTransferAttrs =
  do ca <- coreAttributes
     pa <- presentationAttributes
     fpa <- filterPrimitiveAttributes
     l <- mapM optionalAttr
      ["class","style","in"]
     return $ (\[class_,style,in1] -> (ca,pa,fpa,class_,style,in1) ) l
feCompositeAttrs =
  do ca <- coreAttributes
     pa <- presentationAttributes
     fpa <- filterPrimitiveAttributes
     l <- mapM optionalAttr
      ["class","style","in","in2","operator","k1","k2","k3","k4"]
     return $ (\[class_,style,in1,in2,operator,k1,k2,k3,k4] -> (ca,pa,fpa,class_,style,in1,in2,operator,k1,k2,k3,k4) ) l

feConvolveMatrixAttrs =
  do ca <- coreAttributes
     pa <- presentationAttributes
     fpa <- filterPrimitiveAttributes
     l <- mapM optionalAttr
      ["class","style","in","order","kernelMatrix","divisor","bias","targetX","targetY","edgeMode","kernelUnitLength","preserveAlpha"]
     return $ (\[class_,style,order,km,d,bias,tx,ty,em,ku,pa] -> (ca,pa,fpa,class_,style,order,km,d,bias,tx,ty,em,ku,pa) ) l

feDiffuseLightingAttrs =
  do ca <- coreAttributes
     pa <- presentationAttributes
     fpa <- filterPrimitiveAttributes
     l <- mapM optionalAttr
      ["class","style","in","surfaceScale","diffuseConstant","kernelUnitLength"]
     return $ (\[class_,style,in1,surfaceScale,diffuseConstant,kuLength] -> (ca,pa,fpa,class_,style,in1,surfaceScale,diffuseConstant,kuLength) ) l

feDisplacementMapAttrs =
  do ca <- coreAttributes
     pa <- presentationAttributes
     fpa <- filterPrimitiveAttributes
     l <- mapM optionalAttr
      ["class","style","in","in2","scale","xChannelSelector","yChannelSelector"]
     return $ (\[class_,style,in1,in2,sc,xChan,yChan] -> (ca,pa,fpa,class_,style,in1,in2,sc,xChan,yChan) ) l

feFloodAttrs =
  do ca <- coreAttributes
     pa <- presentationAttributes
     fpa <- filterPrimitiveAttributes
     l <- mapM optionalAttr
      ["class","style"]
     return $ (\[class_,style] -> (ca,pa,fpa,class_,style) ) l

feGaussianBlurAttrs =
  do ca <- coreAttributes
     pa <- presentationAttributes
     fpa <- filterPrimitiveAttributes
     l <- mapM optionalAttr
      ["class","style","in","stdDeviation"]
     return $ (\[class_,style,in1,stdDeviation] -> (ca,pa,fpa,class_,style,in1,stdDeviation) ) l

feImageAttrs =
  do ca <- coreAttributes
     pa <- presentationAttributes
     fpa <- filterPrimitiveAttributes
     xlink <- xlinkAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","preserveAspectRatio"]
     return $ (\[class_,style,ext,pa] -> (ca,pa,fpa,xlink,class_,style,ext,pa) ) l

feMergeAttrs =
  do ca <- coreAttributes
     pa <- presentationAttributes
     fpa <- filterPrimitiveAttributes
     l <- mapM optionalAttr
      ["class","style"]
     return $ (\[class_,style] -> (ca,pa,fpa,class_,style) ) l

feMorphologyAttrs =
  do ca <- coreAttributes
     pa <- presentationAttributes
     fpa <- filterPrimitiveAttributes
     l <- mapM optionalAttr
      ["class","style","in","operator","radius"]
     return $ (\[class_,style,in1,operator,radius] -> (ca,pa,fpa,class_,style,in1,operator,radius) ) l

feOffsetAttrs =
  do ca <- coreAttributes
     pa <- presentationAttributes
     fpa <- filterPrimitiveAttributes
     l <- mapM optionalAttr
      ["class","style","in","dx","dy"]
     return $ (\[class_,style,in1,dx,dy] -> (ca,pa,fpa,class_,style,in1,dx,dy) ) l

feSpecularLightingAttrs =
  do ca <- coreAttributes
     pa <- presentationAttributes
     fpa <- filterPrimitiveAttributes
     l <- mapM optionalAttr
      ["class","style","in","surfaceScale","specularConstant","specularExponent","kernelUnitLength"]
     return $ (\[class_,style,in1,surfaceScale,sc,se,ku] -> (ca,pa,fpa,class_,style,in1,surfaceScale,sc,se,ku) ) l

feTileAttrs =
  do ca <- coreAttributes
     pa <- presentationAttributes
     fpa <- filterPrimitiveAttributes
     l <- mapM optionalAttr
      ["class","style","in"]
     return $ (\[class_,style,in1] -> (ca,pa,fpa,class_,style,in1) ) l

feTurbulenceAttrs =
  do ca <- coreAttributes
     pa <- presentationAttributes
     fpa <- filterPrimitiveAttributes
     l <- mapM optionalAttr
      ["class","style","in","in2","mode"]
     return $ (\[class_,style,in1,in2,mode] -> (ca,pa,fpa,class_,style,in1,in2,mode) ) l

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
     let ns = -- Debug.Trace.trace ("tree" ++ show  tree ++ "\n") $
              nodes tree
     let hashmap = H.fromList ns -- needed because of the use-tag and clipPath
     let image = (scaleY (-1)) (insertRefs hashmap tree) -- insert references from hashmap into tree
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

-------------------------------------------------------------------------------------
-- | A tree structure is needed to handle refences to parts of the tree itself. The \<defs\>-section contains shapes that can be refered to, but the SVG standard allows to refer to every tag in the SVG-file.
-- 
data Tag = Leaf Id ClipRef (Path R2) (Diagram B R2)-- ^A leaf consists of
--
-- * An Id
--
-- * Maybe a reference to a clipPath to clip this leaf
--
-- * A path so that this leaf can be used to clip some other part of a tree
--
-- * A diagram (Another option would have been to apply a function to the upper path)
         | Reference Id Id ClipRef ((Diagram B R2) -> (Diagram B R2)) -- ^ A reference (\<use\>-tag) consists of:
--
-- * An Id
--
-- * A reference to an Id
--
-- * Maybe a clipPath
--
-- * Transformations applied to the reference
         | SubTree Bool Id ClipRef ((Diagram B R2) -> (Diagram B R2)) [Tag]-- ^ A subtree consists of:
--
-- * A Bool: Are we in a section that will be rendered directly (not in a \<defs\>-section)
--
-- * An Id of subdiagram
--
-- * Maybe a clipPath
--
-- * A transformation or application of a style to a subdiagram
--
-- * A list of subtrees

type Id      = Maybe T.Text
type ClipRef = Maybe T.Text

instance Show Tag where
  show (Leaf id1 clip path diagram)  = "Leaf "      ++ (show id1) ++ (show clip) ++ (show path) ++ "\n"
  show (Reference selfid id1 clip f) = "Reference " ++ (show id1) ++ (show clip) ++ "\n"
  show (SubTree b id1 clip f tree)   = "Sub "       ++ (show id1) ++ (show clip) ++ concat (map show tree) ++ "\n"

----------------------------------------------------------------------------------
-- | Put every subtree or leaf that has an id into a list of references to them
nodes :: Tag -> [(T.Text, Tag)]
nodes (Leaf id1 clipref path diagram)  | isJust id1 = [(fromJust id1, Leaf id1 clipref path diagram)]
                                       | otherwise  = []
nodes (Reference selfId id1 clipref f) = []
nodes (SubTree b id1 clipref f children)
     | isJust id1 = [(fromJust id1, SubTree b id1 clipref f children)] ++ (concat (map nodes children))
     | otherwise  =                                                   (concat (map nodes children))

getId :: Tag -> Id
getId (Leaf      id1 clipref path diagram) = id1
getId (Reference id1 ref clipref f)        = id1
getId (SubTree     b id1 clipref f children)   = id1


-- lookup a diagram
lookUp hmap i | isJust l  = fromJust l
              | otherwise = Leaf Nothing Nothing mempty mempty -- an empty diagram if we can't find the id
  where l = H.lookup i hmap

insertRefs :: H.HashMap T.Text Tag -> Tag -> Diagram B R2
insertRefs hmap (Leaf             id1 clipRef path diagram) = diagram # clipByRef hmap clipRef
insertRefs hmap (Reference selfId id1 clipRef f)            = -- Debug.Trace.trace (show (lookUp hmap (fragment id1))) $
                                                              f $ insertRefs hmap (lookUp hmap (fragment id1)) # clipByRef hmap clipRef
insertRefs hmap (SubTree True id1     clipRef f children)   = f (mconcat (map (insertRefs hmap) children)) # clipByRef hmap clipRef
insertRefs hmap (SubTree False _ _ _ _)                     = mempty

clipByRef hmap ref | isJust l  = -- Debug.Trace.trace (show $ evalPath hmap (fromJust l)) $
                                 clipBy $ evalPath hmap (fromJust l)
                   | otherwise = -- Debug.Trace.trace (show ref) $
                                 id
  where l = H.lookup (fragment ref) hmap

evalPath :: H.HashMap T.Text Tag -> Tag -> Path R2
evalPath hmap (Leaf             id1 clipRef path diagram)   = path
evalPath hmap (Reference selfId id1 clipRef f)              = evalPath hmap (lookUp hmap (fragment id1))
evalPath hmap (SubTree _            id1 clipRef f children) = mconcat (map (evalPath hmap) children)

-------------------------------------------------------------------------------------
-- Basic SVG structure

-- | Parse \<svg\>, see <http://www.w3.org/TR/SVG/struct.html#SVGElement>
parseSVG :: MonadThrow m => Sink Event m (Maybe Tag)
parseSVG = tagName "svg" svgAttrs $
   \(cpa,ca,gea,pa,class_,style,ext,x,y,w,h,view,ar,zp,ver,baseprof,cScripT,cStyleT,xmlns,xml) ->
   do gs <- many svgContent
      let st = (parseStyles style) ++ (parsePA pa)
      return $ SubTree True (id1 ca)
                            (clipPath pa)
                            (applyStyleSVG st)
                            (reverse gs)

svgContent = choose
     [parseDesc,parseMetaData,parseTitle, -- descriptive Elements
      parseRect, parseCircle, parseEllipse, parseLine, parsePolyLine, parsePolygon, parsePath, -- shape elements
      parseG, parseDefs, parseSymbol, parseUse, -- structural elements
      parseClipPath, parseImage, parseSwitch, parseText, parsePattern, parseSodipodi]

---------------------------------------------------------------------------
-- | Parse \<g\>, see <http://www.w3.org/TR/SVG/struct.html#GElement>
parseG :: MonadThrow m => Consumer Event m (Maybe Tag)
parseG = tagName "g" gAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr) ->
   do insideGs <- many gContent
      let st = (parseStyles style) ++ (parsePA pa)
      return $ SubTree True (id1 ca)
                            (clipPath pa)
                            ( (applyTr (parseTr tr)) . (applyStyleSVG st) )
                            (reverse insideGs)

gContent = choose [parseDesc,parseTitle, -- descriptive Elements
      parseRect, parseCircle, parseEllipse, parseLine, parsePolyLine, parsePolygon, parsePath, -- shape elements
      parseG, parseDefs, parseSymbol, parseUse, -- structural elements
      parseClipPath, parseImage, parseSwitch, parseText, parsePattern]

---------------------------------------------------------------------------
-- | Parse \<defs\>, see <http://www.w3.org/TR/SVG/struct.html#DefsElement>
parseDefs :: MonadThrow m => Consumer Event m (Maybe Tag)
parseDefs = tagName "defs" gAttrs $
   \(cpa,ca,gea,pa,class_,style,ext,tr) ->
   do insideDefs <- many gContent
      let st = (parseStyles style) ++ (parsePA pa)
      return $ SubTree False (id1 ca)
                             (clipPath pa)
                             ( (applyTr (parseTr tr)) . (applyStyleSVG st) )
                             (reverse insideDefs)

-----------------------------------------------------------------------------------
-- | Parse \<symbol\>, see <http://www.w3.org/TR/SVG/struct.html#SymbolElement>
parseSymbol :: MonadThrow m => Consumer Event m (Maybe Tag)
parseSymbol = tagName "symbol" symbolAttrs $
   \(ca,gea,pa,class_,style,ext,ar,viewbox) ->
   do insideSym <- many symbolContent
      let st = (parseStyles style) ++ (parsePA pa)
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
      let st = (parseStyles style) ++ (parsePA pa)
      return $ Reference (id1 ca) (xlinkHref xlink) (clipPath pa)
                         ( (translate (r2 (p x, p y))) . (applyTr (parseTr tr)) . (applyStyleSVG st) )

useContent = choose [parseDesc,parseTitle] -- descriptive elements

--------------------------------------------------------------------------------------
-- | Parse \<switch\>, see <http://www.w3.org/TR/SVG/struct.html#SwitchElement>
parseSwitch = tagName "switch" switchAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr) ->
   do insideSwitch <- many switchContent
      return $ Leaf (id1 ca) (clipPath pa) mempty mempty

switchContent = choose [parseRect, parseCircle, parseEllipse, parseLine, parsePolyLine, parsePolygon, parsePath]

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
   do return $ Leaf (Just "") Nothing mempty mempty

-----------------------------------------------------------------------------------
-- | Parse \<rect\>,  see <http://www.w3.org/TR/SVG11/shapes.html#RectElement>
parseRect :: MonadThrow m => Consumer Event m (Maybe Tag)
parseRect = tagName "rect" rectAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,ar,tr,x,y,w,h,rx,ry) -> do
     let st = (parseStyles style) ++ (parsePA pa)
     return $ Leaf (id1 ca)
                   (clipPath pa)
                   (path x y w h rx ry tr)
                   (path x y w h rx ry tr # stroke # applyStyleSVG st)

  where path x y w h rx ry tr = (rRect (p w) (p h) (p rx) (p ry)) # alignBL # applyTr (parseTr tr) # translate (r2 (p x, p y))
        rRect pw ph prx pry | prx == 0 && pry == 0 = rect pw ph :: Path R2
                            | otherwise = roundedRect pw ph (if prx == 0 then pry else prx) :: Path R2

---------------------------------------------------------------------------------------------------
-- | Parse \<circle\>,  see <http://www.w3.org/TR/SVG11/shapes.html#CircleElement>
parseCircle :: MonadThrow m => Consumer Event m (Maybe Tag)
parseCircle = tagName "circle" circleAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr,r,cx,cy) -> do
     let st = (parseStyles style) ++ (parsePA pa)
     return $ Leaf (id1 ca)
                   (clipPath pa)
                   (path cx cy r tr)
                   (path cx cy r tr # stroke # applyStyleSVG st )

  where path cx cy r tr = circle (p r) # applyTr (parseTr tr) # translate (r2 (p cx, p cy))

---------------------------------------------------------------------------------------------------
-- | Parse \<ellipse\>,  see <http://www.w3.org/TR/SVG11/shapes.html#EllipseElement>
parseEllipse :: MonadThrow m => Consumer Event m (Maybe Tag)
parseEllipse = tagName "ellipse" ellipseAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr,rx,ry,cx,cy) -> do
     let st = (parseStyles style) ++ (parsePA pa)
     return $ Leaf (id1 ca)
                   (clipPath pa)
                   (path cx cy rx ry tr)
                   (path cx cy rx ry tr # stroke # applyStyleSVG st)

  where path cx cy rx ry tr = ellipseXY (p rx) (p ry) # applyTr (parseTr tr) # translate (r2 (p cx, p cy))

---------------------------------------------------------------------------------------------------
-- | Parse \<line\>,  see <http://www.w3.org/TR/SVG11/shapes.html#LineElement>
parseLine :: MonadThrow m => Consumer Event m (Maybe Tag)
parseLine = tagName "line" lineAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr,x1,y1,x2,y2) -> do
     let st = (parseStyles style) ++ (parsePA pa)
     return $ Leaf (id1 ca)
                   (clipPath pa)
                   (path x1 y1 x2 y2 tr)
                   (path x1 y1 x2 y2 tr # stroke # applyStyleSVG st)

  where path x1 y1 x2 y2 tr =
               fromSegments [ straight (r2 ((p x2) - (p x1), (p y2) - (p y1))) ]
               # applyTr (parseTr tr)
               # translate (r2 (p x1, p y1))

---------------------------------------------------------------------------------------------------
-- | Parse \<polyline\>,  see <http://www.w3.org/TR/SVG11/shapes.html#PolylineElement>
parsePolyLine :: MonadThrow m => Consumer Event m (Maybe Tag)
parsePolyLine = tagName "polyline" polygonAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr,points) -> do
     let st = (parseStyles style) ++ (parsePA pa)
     let ps = parsePoints (fromJust points)
     return $ Leaf (id1 ca)
                   (clipPath pa)
                   (path  ps tr)
                   (dia   ps tr # applyStyleSVG st)

  where path ps tr = fromVertices (map p2 ps)              # applyTr (parseTr tr) # translate (r2 (head ps))
        dia  ps tr = fromVertices (map p2 ps) # strokeLine # applyTr (parseTr tr) # translate (r2 (head ps))

--------------------------------------------------------------------------------------------------
-- | Parse \<polygon\>,  see <http://www.w3.org/TR/SVG11/shapes.html#PolygonElement>
parsePolygon :: MonadThrow m => Consumer Event m (Maybe Tag)
parsePolygon = tagName "polygon" polygonAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr,points) -> do
     let st = (parseStyles style) ++ (parsePA pa)
     let ps = parsePoints (fromJust points)
     return $ Leaf (id1 ca)
                   (clipPath pa)
                   (path  ps tr)
                   (dia   ps tr # applyStyleSVG st)

  where path ps tr = fromVertices (map p2 ps)                          # applyTr (parseTr tr) # translate (r2 (head ps))
        dia  ps tr = fromVertices (map p2 ps) # closeLine # strokeLoop # applyTr (parseTr tr) # translate (r2 (head ps))

--------------------------------------------------------------------------------------------------
-- | Parse \<path\>,  see <http://www.w3.org/TR/SVG11/paths.html#PathElement>
parsePath :: MonadThrow m => Consumer Event m (Maybe Tag)
parsePath = tagName "path" pathAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr,d,pathLength) -> do
     let st = (parseStyles style) ++ (parsePA pa)
     return $ Leaf (id1 ca)
                   (clipPath pa)
                   (path d tr)
                   (path d tr # stroke # applyStyleSVG st)

  where path d tr = (mconcat $ commandsToTrails $ commands d) # applyTr (parseTr tr)

-------------------------------------------------------------------------------------------------
-- | Parse \<clipPath\>, see <http://www.w3.org/TR/SVG/masking.html#ClipPathElement>
parseClipPath :: MonadThrow m => Consumer Event m (Maybe Tag)
parseClipPath = tagName "clipPath" clipPathAttrs $
   \(cpa,ca,pa,class_,style,ext,ar,viewbox) ->
   do insideClipPath <- many clipPathContent
      let st = (parseStyles style) ++ (parsePA pa)
      return $ SubTree False (id1 ca) (clipPath pa) (applyStyleSVG st) (reverse insideClipPath)

clipPathContent = choose [parseRect, parseCircle, parseEllipse, parseLine, parsePolyLine, parsePolygon, parsePath,
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
   do return $ Leaf (id1 ca) (clipPath pa) mempty mempty

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

------------------------------------------------------------------------------------

gradientElements = []
