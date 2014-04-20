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

module Diagrams.SVG.ReadSVG where

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
import Diagrams.SVG.Attributes (applyTr, parseTr, applyStyleSVG, parseStyles, parseDouble, parsePoints, parsePA, CoreAttributes(..), ConditionalProcessingAttributes(..), DocumentEventAttributes(..), GraphicalEventAttributes(..), PresentationAttributes(..), XlinkAttributes(..), FilterPrimitiveAttributes(..), PreserveAR(..), AlignSVG(..), Place(..), MeetOrSlice(..))
import Diagrams.SVG.Path (commands, commandsToTrails, PathCommand(..))
import Prelude hiding (FilePath)
import Filesystem.Path (FilePath)
import Text.XML.Stream.Parse hiding (parseText)

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

xlinkAttributes =
  do l <- mapM optionalAttr
      [ "xlink:href", "xlink:show", "xlink:actuate", "xlink:type", "xlink:role", "xlink:arcrole", 
        "xlink:title"]
     return $ (\[a,b,c,d,e,f,g] -> GEA a b c d e f g) l

filterPrimitiveAttributes =
  do l <- mapM optionalAttr
      [ "x","y","widh","height","result"]
     return $ (\[x,y,w,h,r] -> FPA x y w h r) l

--------------------------------------------------------------------------------------
-- Attributes for basic structure tags, see http://www.w3.org/TR/SVG/struct.html
--------------------------------------------------------------------------------------

svgAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","x","y","width","height","viewBox","preserveAspectRatio",
       "zoomAndPan", "xmlns", "version", "baseProfile", "contentScriptType", "contentStyleType", "xmlns:xlink"]
     return $ (\[class_,style,ext,x,y,w,h,view,ar,zp,xmlns,ver,baseprof,cScripT,cStyleT,href] -> 
              (cpa,ca,gea,pa,class_,style,ext,x,y,w,h,view,ar,zp,xmlns,ver,baseprof,cScripT,cStyleT,href)) l

gAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     class_ <- optionalAttr "class"
     tr <- optionalAttr "transform"
     style <- optionalAttr "style"
     return (cpa,ca,gea,pa,class_,tr,style)

defsAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     class_ <- optionalAttr "class"
     style <- optionalAttr "style"
     ext <- optionalAttr "externalResourceRequired"
     tr <- optionalAttr "transform"
     return (cpa,ca,gea,pa,class_,style,ext,tr)

descAttrs =
  do ca <- coreAttributes
     class_ <- optionalAttr "class"
     style <- optionalAttr "style"
     return (ca,class_,style)	 

symbolAttrs =
  do ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","preserveAspectRatio","viewBox"]
     return $ (\[class_,style,ext,ar,viewbox] -> 
               (ca,gea,pa,class_,style,ext,ar,viewbox) ) l

useAttrs =
  do ca <- coreAttributes
     cpa <- conditionalProcessingAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     xlink <- xlinkAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","transform","x","y","width","height","xlink:href"]
     return $ (\[class_,style,ext,tr,x,y,w,h,href] -> 
      (ca,cpa,gea,pa,xlink,class_,style,ext,tr,x,y,w,h,href)) l
	 
switchAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     class_ <- optionalAttr "class"
     style <- optionalAttr "style"
     ext <- optionalAttr "externalResourcesRequired"
     tr <- optionalAttr "transform"
     return (cpa,ca,gea,pa,class_,style,ext,tr)

--------------------------------------------------------------------------------------
-- Attributes for basic shape tags
--------------------------------------------------------------------------------------

rectAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","preserveAspectRatio","transform","x","y",
       "width","height","rx","ry"]
     return $ (\[class_,style,ext,ar,tr,x,y,w,h,rx,ry] -> 
               (cpa,ca,gea,pa,class_,style,ext,ar,tr,x,y,w,h,rx,ry) ) l

circleAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","transform","r","cx","cy"]
     return $ (\[class_,style,ext,tr,r,cx,cy] -> 
               (cpa,ca,gea,pa,class_,style,ext,tr,r,cx,cy) ) l

ellipseAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","transform","rx","ry","cx","cy"]
     return $ (\[class_,style,ext,tr,rx,ry,cx,cy] -> 
               (cpa,ca,gea,pa,class_,style,ext,tr,rx,ry,cx,cy) ) l

lineAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","transform","x1","y1","x2","y2"]
     return $ (\[class_,style,ext,tr,x1,y1,x2,y2] -> 
               (cpa,ca,gea,pa,class_,style,ext,tr,x1,y1,x2,y2) ) l

polygonAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","transform","points"]
     return $ (\[class_,style,ext,tr,points] -> 
               (cpa,ca,gea,pa,class_,style,ext,tr,points) ) l

pathAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","transform","d","pathLength"]
     return $ (\[class_,style,ext,tr,d,pathLength] -> 
               (cpa,ca,gea,pa,class_,style,ext,tr,d,pathLength) ) l

-------------------------------------------------------------------------------------

clipPathAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","transform","clipPathUnits"]
     return $ (\[class_,style,ext,tr,units] -> 
               (cpa,ca,pa,class_,style,ext,tr,units) ) l

patternAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","viewBox","preserveAspectRatio","x","y",
       "width","height","patternUnits","patternContentUnits","patternTransform"]
     return $ (\[class_,style,ext,view,ar,x,y,w,h,pUnits,pCUnits,pTrans] -> 
               (cpa,ca,pa,class_,style,ext,view,ar,x,y,w,h,pUnits,pCUnits,pTrans) ) l

imageAttrs =
  do ca <- coreAttributes
     cpa <- conditionalProcessingAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","preserveAspectRatio","transform",
       "x","y","width","height","href"]
     return $ (\[class_,style,ext,ar,tr,x,y,w,h,href] -> 
               (ca,cpa,gea,pa,class_,style,ext,ar,tr,x,y,w,h,href) ) l

filterAttrs =
  do ca <- coreAttributes
     pa <- presentationAttributes
     xlink <- xlinkAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","x","y","width","height","filterRes","filterUnits","primitiveUnits","href"]
     return $ (\[class_,style,ext,x,y,w,h,filterRes,filterUnits,primUnits,href] -> 
                (ca,pa,xlink,class_,style,ext,x,y,w,h,filterRes,filterUnits,primUnits,href) ) l

textAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     gea <- graphicalEventAttributes
     pa <- presentationAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","transform","lengthAdjust",
       "x","y","dx","dy","rotate","textLength"]
     return $ (\[class_,style,ext,tr,la,x,y,dx,dy,rot,textlen] -> 
               (cpa,ca,gea,pa,class_,style,ext,tr,la,x,y,dx,dy,rot,textlen) ) l

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
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","preserveAspectRatio","href"]
     return $ (\[class_,style,ext,pa,href] -> (ca,pa,fpa,class_,style,ext,pa,href) ) l

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
-- main library function
--------------------------------------------------------------------------------------

readSVGFile :: FilePath -> Double -> Double -> PreserveAR -> IO (Diagram B R2)
readSVGFile fp width height preserveAR =
  do tree <- runResourceT $ parseFile def fp $$ force "svg tag required" parseSVG 
              -- (C.map stripNamespace parseSVG)
     let hashmap = H.fromList (nodes tree) -- needed because of the use-tag
     let image = (scaleY (-1)) (eval hashmap tree)
     return (preserveAspectRatio width height preserveAR image)

-- according to http://www.w3.org/TR/SVG11/coords.html#PreserveAspectRatioAttribute
-- To Do: implement slice
preserveAspectRatio :: Double -> Double -> PreserveAR -> Diagram B R2 -> Diagram B R2
preserveAspectRatio newWidth newHeight (PAR alignXY Meet) image
   | aspectRatio < newAspectRatio = xPlace alignXY image
   | otherwise                    = yPlace alignXY image
  where w = Diagrams.TwoD.Size.width  image
        h = Diagrams.TwoD.Size.height image
        aspectRatio = w / h
        newAspectRatio = newWidth / newHeight
        scaX = newHeight / h
        scaY = newWidth / w
        xPlace (AlignXY PMin y) i = i # scale scaX # alignBL -- # showOrigin
        xPlace (AlignXY PMid y) i = i # scale scaX # alignBL # translateX ((newWidth - w*scaX)/2) -- # showOrigin
        xPlace (AlignXY PMax y) i = i # scale scaX # alignBL # translateX (newWidth - w*scaX) -- # showOrigin
        yPlace (AlignXY x PMin) i = i # scale scaY # alignBL # translateY (newHeight - h*scaY) -- # showOrigin
        yPlace (AlignXY x PMid) i = i # scale scaY # alignBL # translateY ((newHeight - h*scaY)/2) -- # showOrigin
        yPlace (AlignXY x PMax) i = i # scale scaY # alignBL -- # showOrigin

-------------------------------------------------------------------------------------
-- The tree
-- The Tag structure is needed because of the <use>-tag
-- A reference to another object anywhere in the svg file (objects not restricted to the defs tags)
-------------------------------------------------------------------------------------

type Id = T.Text
type SelfId = T.Text

data Tag = Leaf Id (Diagram B R2) -- leafs of the tree consist of basic shapes
         | Reference SelfId Id ((Diagram B R2) -> (Diagram B R2))

         -- Bool = Are we in a section that will be rendered directly (not in a <defs> section)
         -- Apply a transformation or style to a subdiagram
         | Sub Bool Id ((Diagram B R2) -> (Diagram B R2)) [Tag]

-- lookup a diagram
lookUp hmap i | isJust l  = fromJust l
              | otherwise = Le mempty -- an empty diagram if we can#t find the id
  where l = H.lookup i hmap

eval :: H.HashMap T.Text Ref -> Tag -> Diagram B R2
eval hmap (Leaf id_ diagram)        = diagram
eval hmap (Reference selfId id_ f)  = evalRef hmap (lookUp hmap id_)
eval hmap (Sub b id_ f children) | b == True = f (mconcat (map (eval hmap) children))
                                 | otherwise = mempty

evalRef hmap (Su f list) = f (mconcat (map (evalRef hmap) (map (lookUp hmap) list)))
evalRef hmap (Le d) = d

----------------------------------------------------------------------------------
-- Tag is a tree structure, but we also need a map of references to diagrams

data Ref = Le (Diagram B R2) | Su ((Diagram B R2) -> (Diagram B R2)) [T.Text]

-- flatten a tree into nodes
nodes :: Tag -> [(Id, Ref)]
nodes (Leaf id_ diagram)       = [(id_, Le diagram)]
nodes (Reference selfId id_ f) = []
nodes (Sub b id_ f children)   = [(id_, Su f (map getId children))] ++ (concat (map nodes children))

getId :: Tag -> T.Text
getId (Leaf      id_ diagram)    = id_
getId (Reference id_ ref f)      = id_
getId (Sub     b id_ f children) = id_

-------------------------------------------------------------------------------------
-- Basic SVG structure
-------------------------------------------------------------------------------------

parseSVG :: MonadThrow m => Sink Event m (Maybe Tag)
parseSVG = tagName "svg" svgAttrs $
   \(cpa,ca,gea,pa,class_,style,ext,x,y,w,h,view,ar,zp,xmlns,ver,baseprof,cScriptT,cStyleT,href) ->
   do gs <- many svgContent
      let st = (parseStyles style) ++ (parsePA pa)
      return $ Sub True (fromMaybe T.empty (id_ ca))
                      ( (applyStyleSVG st) )
                       (reverse gs)

svgContent = choose [parseDesc,parseMetaData,parseTitle, -- descriptive Elements
      parseRect, parseCircle, parseEllipse, parseLine, parsePolyLine, parsePolygon, parsePath, -- shape elements
      parseG,parseDefs,parseSymbol,parseUse, -- structural elements
      parseClipPath, parseImage, parseSwitch, parseText, parsePattern]

---------------------------------------------------------------------------

parseG :: MonadThrow m => Consumer Event m (Maybe Tag)
parseG = tagName "g" gAttrs
   $ \(cpa,ca,gea,pa,class_,tr,style) ->
   do insideGs <- many svgContent
      let st = (parseStyles style) ++ (parsePA pa)
      return $ Sub True (fromMaybe T.empty (id_ ca))
                      ( (applyTr (parseTr tr)) . (applyStyleSVG st) )
                      (reverse insideGs)

---------------------------------------------------------------------------

parseDefs :: MonadThrow m => Consumer Event m (Maybe Tag)
parseDefs = tagName "defs" defsAttrs $
   \(cpa,ca,gea,pa,class_,style,ext,tr) ->
   do insideDefs <- many svgContent
      let st = (parseStyles style) ++ (parsePA pa)
      return $ Sub False (fromMaybe T.empty (id_ ca))
                       ( (applyTr (parseTr tr)) . (applyStyleSVG st) )
                       (reverse insideDefs)

-----------------------------------------------------------------------------------
-- http://www.w3.org/TR/SVG/struct.html#SymbolElement
-----------------------------------------------------------------------------------

parseSymbol :: MonadThrow m => Consumer Event m (Maybe Tag)
parseSymbol = tagName "symbol" symbolAttrs $
   \(ca,gea,pa,class_,style,ext,ar,viewbox) ->
   do insideSym <- many svgContent
      let st = (parseStyles style) ++ (parsePA pa)
      return $ Sub False (fromMaybe T.empty (id_ ca)) (applyStyleSVG st) (reverse insideSym)

-----------------------------------------------------------------------------------

parseUse = tagName "use" useAttrs
   $ \(ca,cpa,gea,pa,xlink,class_,style,ext,tr,x,y,w,h,href) ->
   do insideUse <- many useContent
      let st = (parseStyles style) ++ (parsePA pa)
      return $ Reference (fromMaybe T.empty (id_ ca))
                         (fromMaybe T.empty href)
                         ( (applyTr (parseTr tr)) . (applyStyleSVG st) )

useContent = choose [parseDesc,parseMetaData,parseTitle] -- descriptive elements

--------------------------------------------------------------------------------------

parseSwitch = tagName "switch" switchAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr) ->
   do insideSwitch <- many switchContent
      return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

switchContent = choose [parseRect, parseCircle, parseEllipse, parseLine, parsePolyLine, parsePolygon, parsePath]

----------------------------------------------------------------------------------------
-- descriptive elements
----------------------------------------------------------------------------------------

parseDesc :: MonadThrow m => Consumer Event m (Maybe Tag)
parseDesc = tagName "desc" descAttrs
   $ \(ca,class_,style) ->
   do desc <- content
      return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

parseMetaData = tagName "metadata" descAttrs
   $ \(ca,class_,style) ->
   do meta <- content
      return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

parseTitle = tagName "title" descAttrs
   $ \(ca,class_,style) ->
   do title <- content
      return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

-----------------------------------------------------------------------------------
-- Basic shapes, see http://www.w3.org/TR/SVG11/shapes.html
-----------------------------------------------------------------------------------

p x = maybe 0 parseDouble x

applyTrans (x,y) = translate (r2 (mx, my))
  where mx = maybe 0 parseDouble x
        my = maybe 0 parseDouble y

parseRect :: MonadThrow m => Consumer Event m (Maybe Tag)
parseRect = tagName "rect" rectAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,ar,tr,x,y,w,h,rx,ry) -> do
     let st = (parseStyles style) ++ (parsePA pa)
     return $ Leaf (fromMaybe T.empty (id_ ca))
                   ((rRect (p w) (p h) (p rx) (p ry))
                    # alignBL
                    # applyStyleSVG st
                    # applyTr (parseTr tr)
                    # applyTrans (x,y))
  where rRect pw ph prx pry | prx == 0 && pry == 0 = rect pw ph :: Diagram B R2
                            | otherwise = roundedRect pw ph (if prx == 0 then pry else prx) :: Diagram B R2

parseCircle = tagName "circle" circleAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr,r,cx,cy) -> do
     let st = (parseStyles style) ++ (parsePA pa)
     return $ Leaf (fromMaybe T.empty (id_ ca))
	               ((circle (p r) :: Diagram B R2)
                    # applyStyleSVG st
                    # applyTr (parseTr tr)
                    # applyTrans (cx,cy))

parseEllipse = tagName "ellipse" ellipseAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr,rx,ry,cx,cy) -> do
     let st = (parseStyles style) ++ (parsePA pa)
     return $ Leaf (fromMaybe T.empty (id_ ca))
                   ((ellipseXY (p rx) (p ry) :: Diagram B R2)
                    # applyStyleSVG st				   
                    # applyTr (parseTr tr)
                    # applyTrans (cx,cy))

parseLine = tagName "line" lineAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr,x1,y1,x2,y2) -> do
     let st = (parseStyles style) ++ (parsePA pa)
     return $ Leaf (fromMaybe T.empty (id_ ca))
                   ((fromSegments [ straight (r2 ((p x2) - (p x1), (p x2) - (p x1))) ] :: Diagram B R2)
                    # applyStyleSVG st
                    # applyTr (parseTr tr)
                    # applyTrans (x1,y1))

parsePolyLine = tagName "polyline" polygonAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr,points) -> do
     let st = (parseStyles style) ++ (parsePA pa)
     let ps = parsePoints (fromJust points)
     return $ Leaf (fromMaybe T.empty (id_ ca))
                   ((strokeLine $ fromVertices (map p2 ps) :: Diagram B R2)
                    # applyStyleSVG st
                    # applyTr (parseTr tr)
                    # translate (r2 (head ps)))

parsePolygon = tagName "polygon" polygonAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr,points) -> do
     let st = (parseStyles style) ++ (parsePA pa)
     let ps = parsePoints (fromJust points)
     return $ Leaf (fromMaybe T.empty (id_ ca))
                   (((strokeLoop $ closeLine $ fromVertices $ map p2 ps) :: Diagram B R2)
                    # applyStyleSVG st
                    # applyTr (parseTr tr)
                    # translate (r2 (head ps)))

parsePath = tagName "path" pathAttrs
   $ \(cpa,ca,gea,pa,class_,style,ext,tr,d,pathLength) -> do
     let st = (parseStyles style) ++ (parsePA pa)
     return $ Leaf (fromMaybe T.empty (id_ ca))
                   (((stroke $ mconcat $ commandsToTrails (commands d)) :: Diagram B R2)
                    # applyStyleSVG st
                    # applyTr (parseTr tr))

--------------------------------------------------------------------------------------
-- sceletons

parseClipPath :: MonadThrow m => Consumer Event m (Maybe Tag)
parseClipPath = tagName "clipPath" clipPathAttrs $
   \(cpa,ca,pa,class_,style,ext,ar,viewbox) ->
   do insideSym <- many clipPathContent
      return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

clipPathContent = choose [parsePath]

parsePattern :: MonadThrow m => Consumer Event m (Maybe Tag)
parsePattern = tagName "pattern" patternAttrs $
   \(cpa,ca,pa,class_,style,ext,view,ar,x,y,w,h,pUnits,pCUnits,pTrans) ->
   do insideSym <- many patternContent
      return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

patternContent = choose [parseImage]

parseFilter :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFilter = tagName "filter" filterAttrs $
   \(ca,pa,xlink,class_,style,ext,x,y,w,h,filterRes,filterUnits,primUnits,href) ->
   do insideSym <- many filterContent
      return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

filterContent = choose [parseDesc,parseMetaData,parseTitle, -- descriptive Elements
    parseFeBlend,parseFeColorMatrix,parseFeComponentTransfer,parseFeComposite,parseFeConvolveMatrix, -- filter primitive elments
    parseFeDiffuseLighting,parseFeDisplacementMap,parseFeFlood,parseFeGaussianBlur,parseFeImage,
    parseFeMerge,parseFeMorphology,parseFeOffset,parseFeSpecularLighting,parseFeTile,parseFeTurbulence]

--------------------------------------------------------------------------------------
-- filter primitives (currently only sceletons)
--------------------------------------------------------------------------------------

parseFeBlend :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeBlend = tagName "feBlend" feBlendAttrs $
   \(ca,pa,fpa,class_,style,in1,in2,mode) ->
   do return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

parseFeColorMatrix :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeColorMatrix = tagName "feColorMatrix" feColorMatrixAttrs $
   \(ca,pa,fpa,class_,style,in1,type1,values) ->
   do return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

parseFeComponentTransfer :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeComponentTransfer = tagName "feComponentTransfer" feComponentTransferAttrs $
   \(ca,pa,fpa,class_,style,in1) ->
   do return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

parseFeComposite :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeComposite = tagName "feComposite" feCompositeAttrs $
   \(ca,pa,fpa,class_,style,in1,in2,operator,k1,k2,k3,k4) ->
   do return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

parseFeConvolveMatrix :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeConvolveMatrix = tagName "feConvolveMatrix" feConvolveMatrixAttrs $
   \(ca,pa,fpa,class_,style,order,km,d,bias,tx,ty,em,ku,par) ->
   do return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

parseFeDiffuseLighting :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeDiffuseLighting = tagName "feDiffuseLighting" feDiffuseLightingAttrs $
   \(ca,pa,fpa,class_,style,in1,surfaceScale,diffuseConstant,kuLength) ->
   do return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

parseFeDisplacementMap :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeDisplacementMap = tagName "feDisplacementMap" feDisplacementMapAttrs $
   \(ca,pa,fpa,class_,style,in1,in2,sc,xChan,yChan) ->
   do return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

parseFeFlood :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeFlood = tagName "feFlood" feFloodAttrs $
   \(ca,pa,fpa,class_,style) ->
   do return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

parseFeGaussianBlur :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeGaussianBlur = tagName "feGaussianBlur" feGaussianBlurAttrs $
   \(ca,pa,fpa,class_,style,in1,stdDeviation) ->
   do return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

parseFeImage :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeImage = tagName "feImage" feImageAttrs $
   \(ca,pa,fpa,class_,style,ext,par,href) ->
   do return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

parseFeMerge :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeMerge = tagName "feMerge" feMergeAttrs $
   \(ca,pa,fpa,class_,style) ->
   do return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

parseFeMorphology :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeMorphology = tagName "feMorphology" feMorphologyAttrs $
   \(ca,pa,fpa,class_,style,in1,operator,radius) ->
   do return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

parseFeOffset :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeOffset = tagName "feOffset" feOffsetAttrs $
   \(ca,pa,fpa,class_,style,in1,dx,dy) ->
   do return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

parseFeSpecularLighting :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeSpecularLighting = tagName "feSpecularLighting" feSpecularLightingAttrs $
   \(ca,pa,fpa,class_,style,in1,surfaceScale,sc,se,ku) ->
   do return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

parseFeTile :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeTile = tagName "feTile" feTileAttrs $
   \(ca,pa,fpa,class_,style,in1) ->
   do return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

parseFeTurbulence :: MonadThrow m => Consumer Event m (Maybe Tag)
parseFeTurbulence = tagName "feTurbulence" feTurbulenceAttrs $
   \(ca,pa,fpa,class_,style,in1,in2,mode) ->
   do return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

--------------------------------------------------------------------------------------

parseImage :: MonadThrow m => Consumer Event m (Maybe Tag)
parseImage = tagName "image" imageAttrs $
   \(ca,cpa,gea,pa,class_,style,ext,ar,tr,x,y,w,h,href) ->
   do return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

parseText :: MonadThrow m => Consumer Event m (Maybe Tag)
parseText = tagName "text" textAttrs $
   \(cpa,ca,gea,pa,class_,style,ext,tr,la,x,y,dx,dy,rot,textlen) ->
   do return $ Leaf (fromMaybe T.empty (id_ ca)) mempty

------------------------------------------------------------------------------------

animationElements = []

------------------------------------------------------------------------------------

gradientElements = []
