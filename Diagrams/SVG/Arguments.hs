{-# LANGUAGE OverloadedStrings #-}

module Diagrams.SVG.Arguments
    (
    -- * Attribute Parsing of classes of attributes
      coreAttributes
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
    , sAttrs
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
    , linearGradAttrs
    , radialGradAttrs
    , setAttrs
    , stopAttrs
    , textAttrs
    , tspanAttrs
    , namedViewAttrs
    , perspectiveAttrs
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
    )
where
import Text.XML.Stream.Parse
import Diagrams.SVG.Attributes

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

-- | Attributes for \<g\> and \<defs\>, see <http://www.w3.org/TR/SVG/struct.html#GElement>
sAttrs =
  do ca <- coreAttributes
     type_ <- optionalAttr "type"
     media <- optionalAttr "media"
     title <- optionalAttr "title"
     ignoreAttrs
     return (ca,type_,media,title)

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

linearGradAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     pa <- presentationAttributes
     xlink <- xlinkAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","x1","y1","x2","y2","gradientUnits","gradientTransform","spreadMethod"]
     ignoreAttrs
     return $        (\[class_,style,ext,x1,y1,x2,y2,gradientUnits,gradientTransform,spreadMethod] -> 
       (cpa,ca,pa,xlink,class_,style,ext,x1,y1,x2,y2,gradientUnits,gradientTransform,spreadMethod) ) l

radialGradAttrs =
  do cpa <- conditionalProcessingAttributes
     ca <- coreAttributes
     pa <- presentationAttributes
     xlink <- xlinkAttributes
     l <- mapM optionalAttr
      ["class","style","externalResourcesRequired","cx","cy","r","fx","fy","gradientUnits","gradientTransform","spreadMethod"]
     ignoreAttrs
     return $        (\[class_,style,ext,cx,cy,r,fx,fy,gradientUnits,gradientTransform,spreadMethod] -> 
       (cpa,ca,pa,xlink,class_,style,ext,cx,cy,r,fx,fy,gradientUnits,gradientTransform,spreadMethod) ) l

setAttrs =
  do ca <- coreAttributes
     pa <- presentationAttributes
     xlink <- xlinkAttributes
     ignoreAttrs
     return (ca,pa,xlink)

stopAttrs =
  do ca <- coreAttributes
     pa <- presentationAttributes
     xlink <- xlinkAttributes
     class_ <- optionalAttr "class"
     style  <- optionalAttr "style"
     offset <- optionalAttr "offset"
     ignoreAttrs
     return $ (ca,pa,xlink,class_,style,offset)

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

tspanAttrs =
  do p <- mapM optionalAttr
      [ "{http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd}role", "id", "x", "y" ]
     ignoreAttrs
     return $ ( \[role,id_,x,y] -> (role,id_,x,y) ) p

namedViewAttrs =
  do l <- mapM optionalAttr
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

{-   <inkscape:perspective
       sodipodi:type="inkscape:persp3d"
       inkscape:vp_x="0 : 212.5 : 1"
       inkscape:vp_y="0 : 1000 : 0"
       inkscape:vp_z="428.75 : 212.5 : 1"
       inkscape:persp3d-origin="214.375 : 141.66667 : 1"
       id="perspective5175" />
-}
perspectiveAttrs =
  do p <- mapM optionalAttr
       [ "{http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd}type",
         "{http://www.inkscape.org/namespaces/inkscape}vp_x",
         "{http://www.inkscape.org/namespaces/inkscape}vp_y",
         "{http://www.inkscape.org/namespaces/inkscape}vp_z",
         "{http://www.inkscape.org/namespaces/inkscape}persp3d-origin",
         "id"]
     ignoreAttrs
     return $ (\[typ,vp_x,vp_y,vp_z,persp3d_origin,id_] -> 
                (typ,vp_x,vp_y,vp_z,persp3d_origin,id_) ) p

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

