{-# LANGUAGE TypeFamilies, OverloadedStrings, FlexibleContexts #-}
--------------------------------------------------------------------
-- |
-- Module    : Diagrams.SVG.Tree
-- Copyright : (c) 2015 Tillmann Vogt <tillk.vogt@googlemail.com>
-- License   : BSD3
--
-- Maintainer: diagrams-discuss@googlegroups.com
-- Stability : stable
-- Portability: portable

module Diagrams.SVG.Tree
    (
    -- * Tree data type
      Tag(..)
    , HashMaps(..)
    -- * Extract data from the tree
    , nodes
    , Attrs(..)
    , NodesMap
    , CSSMap
    , GradientsMap
    , PreserveAR(..)
    , AlignSVG(..)
    , MeetOrSlice(..)
    , Place
    , ViewBox(..)
    , Gr(..)
    , GradientAttributes(..)
    , PresentationAttributes(..)
    , GradRefId
    , expandGradMap
    , insertRefs
    , preserveAspectRatio
    , FontContent(..)
    , FontData(..)
    , FontFace(..)
    , Glyph(..)
    , KernDir(..)
    , KernMaps(..)
    , SvgGlyphs(..)
    , Kern(..)
    )
where
import           Data.Maybe (isJust, fromJust , fromMaybe)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import           Data.Text(Text(..))
import           Data.Vector(Vector)
import           Diagrams.Prelude hiding (Vector)
import           Diagrams.TwoD.Size
-- import           Diagrams.SVG.Fonts.ReadFont
import           Debug.Trace

-- Note: Maybe we could use the Tree from diagrams here but on the other hand this makes diagrams-input 
-- more independent of changes of diagrams' internal structures

-------------------------------------------------------------------------------------
-- | A tree structure is needed to handle refences to parts of the tree itself.
-- The \<defs\>-section contains shapes that can be refered to, but the SVG standard allows to refer to
-- every tag in the SVG-file.
--
data Tag b n = Leaf Id (ViewBox n -> Path V2 n) ((HashMaps b n, ViewBox n) -> Diagram b)-- ^
-- A leaf consists of
--
-- * An Id
--
-- * A path so that this leaf can be used to clip some other part of a tree
--
-- * A diagram (Another option would have been to apply a function to the upper path)
     | Reference Id Id (ViewBox n -> Path V2 n) ((HashMaps b n, ViewBox n) -> Diagram b -> Diagram b)-- ^
--  A reference (\<use\>-tag) consists of:
--
-- * An Id
--
-- * A reference to an Id
--
-- * A viewbox so that percentages are relative to this viewbox
--
-- * Transformations applied to the reference
     | SubTree Bool Id (Double, Double)
                       (Maybe (ViewBox n)) 
                       (Maybe PreserveAR) 
                       (HashMaps b n -> Diagram b -> Diagram b) 
                       [Tag b n]-- ^
-- A subtree consists of:
--
-- * A Bool: Are we in a section that will be rendered directly (not in a \<defs\>-section)
--
-- * An Id of subdiagram
--
-- * A viewbox so that percentages are relative to this viewbox
--
-- * Aspect Ratio
--
-- * A transformation or application of a style to a subdiagram
--
-- * A list of subtrees
     | StyleTag [(Text, [(Text, Text)])] -- ^ A tag that contains CSS styles with selectors and attributes
     | FontTag (FontData b n)
     | Grad Id (Gr n) -- ^ A gradient
     | Stop (HashMaps b n -> [GradientStop n]) -- ^
-- We need to make this part of this data structure because Gradient tags can also contain description tags

type Id        = Maybe Text
type GradRefId = Maybe Text
type Attrs     = [(Text, Text)]

type Nodelist b n = [(Text, Tag b n)]
type CSSlist  = [(Text, Attrs)]
data Gr n = Gr GradRefId
               GradientAttributes
               (Maybe (ViewBox n))
               [CSSMap -> [GradientStop n]]
               (CSSMap -> GradientAttributes -> ViewBox n -> [CSSMap -> [GradientStop n]] -> Texture n)

type Gradlist n = [(Text, Gr n)]
type Fontlist b n = [(Text, FontData b n)]

type HashMaps b n = (NodesMap b n, CSSMap, GradientsMap n)
type NodesMap b n = H.HashMap Text (Tag b n)
type CSSMap = H.HashMap Text Attrs
type GradientsMap n = H.HashMap Text (Gr n)

type ViewBox n = (n,n,n,n) -- (MinX,MinY,Width,Height)

data PreserveAR = PAR AlignSVG MeetOrSlice -- ^ see <http://www.w3.org/TR/SVG11/coords.html#PreserveAspectRatioAttribute>
data AlignSVG = AlignXY Place Place -- ^ alignment in x and y direction
type Place = Double -- ^ A value between 0 and 1, where 0 is the minimal value and 1 the maximal value
data MeetOrSlice = Meet | Slice

instance Show (Tag b n) where
  show (Leaf id1 _ _)  = "Leaf "      ++ (show id1) ++ "\n"
  show (Reference selfid id1 viewbox f) = "Reference " ++ (show id1) ++ "\n"
  show (SubTree b id1 wh viewbox ar f tree) = "Sub " ++ (show id1) ++ concat (map show tree) ++ "\n"
  show (StyleTag _)   = "Style "    ++ "\n"
  show (Grad id1 gr) = "Grad id:" ++ (show id1) -- ++ (show gr) ++ "\n"
  show (Stop _)   = "Stop " ++ "\n"

-- instance Show (Gr n) where show (Gr gradRefId gattr vb stops tex) = "  ref:" ++ (show gradRefId) ++ "viewbox: " ++ (show vb)

----------------------------------------------------------------------------------
-- | Generate elements that can be referenced by their ID.
--   The tree nodes are splitted into 4 groups of lists of (ID,value)-pairs):
--
-- * Nodes that contain elements that can be transformed to a diagram
--
-- * CSS classes with corresponding (attribute,value)-pairs, from the <defs>-tag
--
-- * Gradients
--
-- * Fonts
nodes :: Maybe (ViewBox n) -> (Nodelist b n, CSSlist, Gradlist n, Fontlist b n) -> Tag b n -> 
                              (Nodelist b n, CSSlist, Gradlist n, Fontlist b n)
nodes viewbox (ns,css,grads,fonts) (Leaf id1 path diagram)
  | isJust id1 = (ns ++ [(fromJust id1, Leaf id1 path diagram)],css,grads,fonts)
  | otherwise  = (ns,css,grads,fonts)

-- A Reference element for the <use>-tag
nodes viewbox (ns,css,grads,fonts) (Reference selfId id1 vb f) = (ns,css,grads,fonts)

nodes viewbox (ns,css,grads,fonts)                (SubTree b id1 wh Nothing ar f children)
  | isJust id1 = myconcat [ (ns ++ [(fromJust id1, SubTree b id1 wh viewbox ar f children)],css,grads,fonts) ,
                            (myconcat (map (nodes viewbox (ns,css,grads,fonts)) children))                ]
  | otherwise  = myconcat (map (nodes viewbox (ns,css,grads,fonts)) children)

nodes viewbox (ns,css,grads,fonts)                (SubTree b id1 wh vb ar f children)
  | isJust id1 = myconcat [ (ns ++ [(fromJust id1, SubTree b id1 wh vb ar f children)],css,grads,fonts) ,
                            (myconcat (map (nodes vb (ns,css,grads,fonts)) children))                ]
  | otherwise  = myconcat (map (nodes vb (ns,css,grads,fonts)) children)

nodes viewbox (ns,css,grads,fonts) (Grad id1 (Gr gradRefId gattr vb stops texture))
  | isJust id1 = (ns,css, grads ++ [(fromJust id1, Gr gradRefId gattr vb stops texture)], fonts)
  | otherwise  = (ns,css,grads,fonts)

-- There is a global style tag in the defs section of some svg files
nodes viewbox (ns,css,grads,fonts) (StyleTag styles) = (ns,css ++ styles,grads,fonts)
-- stops are not extracted here but from the gradient parent node
nodes viewbox lists (Stop _) = lists

nodes viewbox (ns,css,grads,fonts) (FontTag fontData) = (ns,css,grads,fonts ++ [(fromMaybe "" (fontId fontData), fontData)])

myconcat :: [(Nodelist b n, CSSlist, Gradlist n, Fontlist b n)] -> (Nodelist b n, CSSlist, Gradlist n, Fontlist b n)
myconcat list = (concat $ map sel1 list, concat $ map sel2 list, concat $ map sel3 list, concat $ map sel4 list)
  where sel1 (a,b,c,d) = a
        sel2 (a,b,c,d) = b
        sel3 (a,b,c,d) = c
        sel4 (a,b,c,d) = d

------------------------------------------------------------------------------------------------------
-- The following code is necessary to handle nested xlink:href in gradients,
-- like in this example (#linearGradient3606 in radialGradient):
--
--    <linearGradient
--       id="linearGradient3606">
--      <stop
--         id="stop3608"
--         style="stop-color:#ff633e;stop-opacity:1"
--         offset="0" />
--      <stop
--         id="stop3610"
--         style="stop-color:#ff8346;stop-opacity:0.78225809"
--         offset="1" />
--    </linearGradient>
--    <radialGradient
--       cx="275.00681"
--       cy="685.96008"
--       r="112.80442"
--       fx="275.00681"
--       fy="685.96008"
--       id="radialGradient3612"
--       xlink:href="#linearGradient3606"
--       gradientUnits="userSpaceOnUse"
--       gradientTransform="matrix(1,0,0,1.049029,-63.38387,-67.864647)" />

-- | Gradients contain references to include attributes/stops from other gradients. 
--   expandGradMap expands the gradient with these attributes and stops

expandGradMap :: GradientsMap n ->  GradientsMap n -- GradientsMap n = H.HashMap Text (Gr n)
expandGradMap gradMap = H.mapWithKey (newGr gradMap) gradMap

newGr grMap key (Gr gradRefId attrs vb stops f) = (Gr gradRefId newAttributes vb newStops f)
  where newStops = stops ++ (gradientStops grMap gradRefId)
        newAttributes = overwriteDefaultAttributes $ gradientAttributes grMap (Just key)

-- | Gradients that reference other gradients form a list of attributes
--   The last element of this list are the default attributes (thats why there is "reverse attrs")
--   Then the second last attributes overwrite these defaults (and so on until the root)
--   The whole idea of this nesting is that Nothing values don't overwrite Just values
overwriteDefaultAttributes :: [GradientAttributes] -> GradientAttributes
overwriteDefaultAttributes [attrs] = attrs
overwriteDefaultAttributes attrs = foldl1 updateRec (reverse attrs)

-- | Every reference is looked up in the gradient map and a record of attributes is added to a list
gradientAttributes :: GradientsMap n -> GradRefId -> [GradientAttributes] -- GradientsMap n = H.HashMap Text (Gr n)
gradientAttributes grMap Nothing = []
gradientAttributes grMap (Just refId) | isJust gr = (attrs $ fromJust gr) : (gradientAttributes grMap (grRef $ fromJust gr))
                                      | otherwise = []
  where gr = H.lookup refId grMap
        grRef   (Gr ref _ _ _ _) = ref

attrs   (Gr _ att _ _ _) = att

-- | Every reference is looked up in the gradient map and the stops are added to a list
gradientStops :: GradientsMap n -> GradRefId -> [CSSMap -> [GradientStop n]]
gradientStops grMap Nothing = []
gradientStops grMap (Just refId) | isJust gr = (stops $ fromJust gr) ++ (gradientStops grMap (grRef $ fromJust gr))
                                 | otherwise = []
  where gr = H.lookup refId grMap
        grRef   (Gr ref _ _ _ _) = ref
        stops   (Gr _  _ _ st _) = st

-- | Update the gradient record. The first argument is the leaf record, the second is the record that overwrites the leaf.
--   The upper example references gradients that have only stops (no overwriting of attributes).
--   See <http://www.w3.org/TR/SVG/pservers.html#RadialGradientElementHrefAttribute>
updateRec :: GradientAttributes -> GradientAttributes -> GradientAttributes
updateRec (GA pa  class_  style  x1  y1  x2  y2  cx  cy  r  fx  fy  gradientUnits  gradientTransform  spreadMethod)
          (GA paN class1N styleN x1N y1N x2N y2N cxN cyN rN fxN fyN gradientUnitsN gradientTransformN spreadMethodN)
  = toGA (paN, (updateList [class_,style,x1,y1,x2,y2,cx,cy,r,fx,fy,gradientUnits,gradientTransform,spreadMethod] -- TODO: update pa
                           [class1N,styleN,x1N,y1N,x2N,y2N,cxN,cyN,rN,fxN,fyN,gradientUnitsN,gradientTransformN,spreadMethodN]))
  where
    updateList :: [Maybe Text] -> [Maybe Text] -> [Maybe Text]
    updateList (defaultt:xs) ((Just t1):ys) = (Just t1) : (updateList xs ys)
    updateList ((Just t0):xs) (Nothing  :ys) = (Just t0) : (updateList xs ys)
    updateList  (Nothing :xs) (Nothing  :ys) =  Nothing  : (updateList xs ys)
    updateList _ _ = []

    toGA (pa, [class_,style,x1,y1,x2,y2,cx,cy,r,fx,fy,gradientUnits,gradientTransform,spreadMethod]) =
       GA pa   class_ style x1 y1 x2 y2 cx cy r fx fy gradientUnits gradientTransform spreadMethod

------------------------------------------------------------------------------------------------------------

-- | Lookup a diagram and return an empty diagram in case the SVG-file has a wrong reference
lookUp hmap i | (isJust i) && (isJust l) = fromJust l
              | otherwise = Leaf Nothing mempty mempty -- an empty diagram if we can't find the id
  where l = H.lookup (fromJust i) hmap

-- | Evaluate the tree into a diagram by inserting xlink:href references from nodes and gradients, 
--   applying clipping and passing the viewbox to the leafs
insertRefs :: (V b ~ V2, N b ~ n, RealFloat n, Place ~ n) => (HashMaps b n, ViewBox n) -> Tag b n -> Diagram b

insertRefs (maps,viewbox) (Leaf id1 path f) = (f (maps,viewbox)) # (if isJust id1 then named (T.unpack $ fromJust id1) else id)
insertRefs (maps,viewbox) (Grad _ _) = mempty
insertRefs (maps,viewbox) (Stop f) = mempty
insertRefs (maps,viewbox) (Reference selfId id1 path styles)
    | (Diagrams.TwoD.Size.width r) <= 0 || (Diagrams.TwoD.Size.height r) <= 0 = mempty
    | otherwise = referencedDiagram # styles (maps,viewbox)
                                    # cutOutViewBox viewboxPAR
--                                    # stretchViewBox (fromJust w) (fromJust h) viewboxPAR
                                    # (if isJust selfId then named (T.unpack $ fromJust selfId) else id)
  where r = path viewbox
        viewboxPAR = getViewboxPreserveAR subTree
        referencedDiagram = insertRefs (maps,viewbox) (makeSubTreeVisible viewbox subTree)
        subTree = lookUp (sel1 maps) id1
        getViewboxPreserveAR (SubTree _ id1 wh viewbox ar g children) = (viewbox, ar)
        getViewboxPreserveAR _ = (Nothing, Nothing)
        sel1 (a,b,c) = a

insertRefs (maps,viewbox) (SubTree False _ _ _ _ _ _) = mempty
insertRefs (maps,viewbox) (SubTree True id1 (w,h) viewb ar styles children) =
    subdiagram # styles maps
               # cutOutViewBox (viewb, ar)
               # (if (w > 0) && (h > 0) then stretchViewBox w h (viewb, ar) else id)
               # (if isJust id1 then named (T.unpack $ fromJust id1) else id)
  where subdiagram = mconcat (map (insertRefs (maps, fromMaybe viewbox viewb)) children)

insertRefs (maps,viewbox) (StyleTag _) = mempty
-------------------------------------------------------------------------------------------------------------------------------

makeSubTreeVisible viewbox (SubTree _    id1 wh vb ar g children) =
                           (SubTree True id1 wh (Just viewbox) ar g (map (makeSubTreeVisible viewbox) children))
makeSubTreeVisible _ x = x

stretchViewBox w h ((Just (minX,minY,width,height), Just par)) = preserveAspectRatio w h (width - minX) (height - minY) par
stretchViewBox w h ((Just (minX,minY,width,height), Nothing))  = -- Debug.Trace.trace "nothing" $
                                    preserveAspectRatio w h (width - minX) (height - minY) (PAR (AlignXY 0.5 0.5) Meet)
stretchViewBox w h _ = id

cutOutViewBox (Just (minX,minY,width,height), _) = rectEnvelope (p2 (minX, minY)) (r2 ((width - minX), (height - minY)))
                                                 --  (clipBy (rect (width - minX) (height - minY)))
cutOutViewBox _ = id

-------------------------------------------------------------------------------------------------------------------------------
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
        xPlace (PAR (AlignXY x y) Meet)  i = i # scale scaX # alignBL # translateX ((newWidth  - oldWidth*scaX)*x)
        xPlace (PAR (AlignXY x y) Slice) i = i # scale scaY # alignBL # translateX ((newWidth  - oldWidth*scaX)*x)
--                                               # view (p2 (0, 0)) (r2 (newWidth, newHeight))

        yPlace (PAR (AlignXY x y) Meet)  i = i # scale scaY # alignBL # translateY ((newHeight - oldHeight*scaY)*y)
        yPlace (PAR (AlignXY x y) Slice) i = i # scale scaX # alignBL # translateY ((newHeight - oldHeight*scaY)*y)
--                                               # view (p2 (0, 0)) (r2 (newWidth, newHeight))


-- a combination of linear- and radial-attributes so that referenced gradients can replace Nothing-attributes
data GradientAttributes =
  GA { presentationAttributes :: PresentationAttributes
     , class_ :: Maybe Text
     , style  :: Maybe Text
     , x1  :: Maybe Text
     , y1  :: Maybe Text
     , x2  :: Maybe Text
     , y2  :: Maybe Text
     , cx  :: Maybe Text
     , cy  :: Maybe Text
     , r   :: Maybe Text
     , fx  :: Maybe Text
     , fy  :: Maybe Text
     , gradientUnits     :: Maybe Text
     , gradientTransform :: Maybe Text
     , spreadMethod      :: Maybe Text
     }

-- GA pa class_ style x1 y1 x2 y2 cx cy r fx fy gradientUnits gradientTransform spreadMethod

data PresentationAttributes =
   PA { alignmentBaseline :: Maybe Text
      , baselineShift :: Maybe Text
      , clip :: Maybe Text
      , clipPath :: Maybe Text
      , clipRule :: Maybe Text
      , color :: Maybe Text
      , colorInterpolation :: Maybe Text
      , colorInterpolationFilters :: Maybe Text
      , colorProfile :: Maybe Text
      , colorRendering :: Maybe Text
      , cursor :: Maybe Text
      , direction :: Maybe Text
      , display :: Maybe Text
      , dominantBaseline :: Maybe Text
      , enableBackground :: Maybe Text
      , fill :: Maybe Text
      , fillOpacity :: Maybe Text
      , fillRuleSVG :: Maybe Text
      , filter :: Maybe Text
      , floodColor :: Maybe Text
      , floodOpacity :: Maybe Text
      , fontFamily :: Maybe Text
      , fntSize :: Maybe Text
      , fontSizeAdjust :: Maybe Text
      , fontStretch :: Maybe Text
      , fontStyle :: Maybe Text
      , fontVariant :: Maybe Text
      , fontWeight :: Maybe Text
      , glyphOrientationHorizontal :: Maybe Text
      , glyphOrientationVertical :: Maybe Text
      , imageRendering :: Maybe Text
      , kerning :: Maybe Text
      , letterSpacing :: Maybe Text
      , lightingColor :: Maybe Text
      , markerEnd :: Maybe Text
      , markerMid :: Maybe Text
      , markerStart :: Maybe Text
      , mask :: Maybe Text
      , opacity :: Maybe Text
      , overflow :: Maybe Text
      , pointerEvents :: Maybe Text
      , shapeRendering :: Maybe Text
      , stopColor :: Maybe Text
      , stopOpacity :: Maybe Text
      , strokeSVG :: Maybe Text
      , strokeDasharray :: Maybe Text
      , strokeDashoffset :: Maybe Text
      , strokeLinecap :: Maybe Text
      , strokeLinejoin :: Maybe Text
      , strokeMiterlimit :: Maybe Text
      , strokeOpacity :: Maybe Text
      , strokeWidth :: Maybe Text
      , textAnchor :: Maybe Text
      , textDecoration :: Maybe Text
      , textRendering :: Maybe Text
      , unicodeBidi :: Maybe Text
      , visibility :: Maybe Text
      , wordSpacing :: Maybe Text
      , writingMode :: Maybe Text
      } deriving Show

type SvgGlyphs n = H.HashMap Text (Maybe Text, n, Maybe Text)
-- ^ \[ (unicode, (glyph_name, horiz_advance, ds)) \]

data Kern n = Kern
  { kernDir :: KernDir
  , kernU1  :: [Text]
  , kernU2  :: [Text]
  , kernG1  :: [Text]
  , kernG2  :: [Text]
  , kernK   :: n
  }

-- | Data from the subtags
data FontContent b n = FF (FontFace n) | GG (Glyph b n) | KK (Kern n)

-- | All data in the \<font\>-tag
data FontData b n = FontData
  {
    fontId                         :: Maybe Text
  , fontDataHorizontalOriginX      :: Maybe Text
  , fontDataHorizontalOriginY      :: Maybe Text
  , fontDataHorizontalAdvance      :: n
  , fontDataVerticalOriginX        :: Maybe Text
  , fontDataVerticalOriginY        :: Maybe Text
  , fontDataVerticalAdvance        :: Maybe Text
  -- ^ data gathered from subtags
  , fontFace                       :: FontFace n
  , fontMissingGlyph               :: Glyph b n
  , fontDataGlyphs                 :: SvgGlyphs n
--  , fontDataRawKernings            :: [(Text, [Text], [Text], [Text], [Text])]
  , fontDataKerning                :: KernMaps n
--  , fontDataFileName               :: Text
}

data FontFace n = FontFace
  { fontDataFamily                 :: Maybe Text
  , fontDataStyle                  :: Maybe Text
  , fontDataVariant                :: Maybe Text
  , fontDataWeight                 :: Maybe Text
  , fontDataStretch                :: Maybe Text
  , fontDataSize                   :: Maybe Text
  , fontDataUnicodeRange           :: Maybe Text
  , fontDataUnitsPerEm             :: Maybe Text
  , fontDataPanose                 :: Maybe Text
  , fontDataVerticalStem           :: Maybe Text
  , fontDataHorizontalStem         :: Maybe Text
  , fontDataSlope                  :: Maybe Text
  , fontDataCapHeight              :: Maybe Text
  , fontDataXHeight                :: Maybe Text
  , fontDataAccentHeight           :: Maybe Text
  , fontDataAscent                 :: Maybe Text
  , fontDataDescent                :: Maybe Text
  , fontDataWidths                 :: Maybe Text
  , fontDataBoundingBox            :: [n]
  , fontDataIdeographicBaseline    :: Maybe Text
  , fontDataAlphabeticBaseline     :: Maybe Text
  , fontDataMathematicalBaseline   :: Maybe Text
  , fontDataHangingBaseline        :: Maybe Text
  , fontDataVIdeographicBaseline   :: Maybe Text
  , fontDataVAlphabeticBaseline    :: Maybe Text
  , fontDataVMathematicalBaseline  :: Maybe Text
  , fontDataVHangingBaseline       :: Maybe Text
  , fontDataUnderlinePos           :: Maybe Text
  , fontDataUnderlineThickness     :: Maybe Text
  , fontDataStrikethroughPos       :: Maybe Text
  , fontDataStrikethroughThickness :: Maybe Text
  , fontDataOverlinePos            :: Maybe Text
  , fontDataOverlineThickness      :: Maybe Text
  }

data Glyph b n = Glyph
  { glyphId     :: Maybe Text
  , glyph       :: Tag b n
  , d           :: Maybe Text
  , horizAdvX   :: n
  , vertOriginX :: n
  , vertOriginY :: n
  , vertAdvY    :: n
  , unicode     :: Maybe Text
  , glyphName   :: Maybe Text
  , orientation :: Maybe Text
  , arabicForm  :: Maybe Text
  , lang        :: Maybe Text
  }

data KernDir = HKern | VKern

data KernMaps n = KernMaps
  { kernDirs :: [KernDir]
  , kernU1S :: H.HashMap Text [Int]
  , kernU2S :: H.HashMap Text [Int]
  , kernG1S :: H.HashMap Text [Int]
  , kernG2S :: H.HashMap Text [Int]
  , kernKs   :: Vector n
  }

