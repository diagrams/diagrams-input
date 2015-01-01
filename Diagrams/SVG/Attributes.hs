{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

--------------------------------------------------------------------
-- |
-- Module    : Diagrams.SVG.Attributes
-- Copyright : (c) 2015 Tillmann Vogt <tillk.vogt@googlemail.com>
-- License   : BSD3
--
-- Maintainer: diagrams-discuss@googlegroups.com
-- Stability : stable
-- Portability: portable

module Diagrams.SVG.Attributes 
    (
      initialStyles
    -- * Classes of attributes
    , CoreAttributes(..)
    , ConditionalProcessingAttributes(..)
    , DocumentEventAttributes(..)
    , GraphicalEventAttributes(..)
    , XlinkAttributes(..)
    , FilterPrimitiveAttributes(..)
    , NameSpaces(..)
    -- * General Parsing Functions
    , separatedBy
    , parseOne
    , parseOne'
    , compose
    , parseDouble
    , parseMaybeDouble
    , parseToDouble
    , parsePoints
    , parseTempl
    , parseIRI
    -- * Transformations
    , applyTr
    , parseTr
    -- * Parsing the style attribute
    , PresentationAttributes(..)
    , applyStyleSVG
    , parseStyles
    , parseLengths
    , parseViewBox
    , parsePA
    , cssStylesFromMap
    , fragment
    , p
    -- * Parsing Colors
    -- * Parsing preserve aspect ratio
    , parsePreserveAR
    , PreserveAR(..)
    , AlignSVG(..)
    , Place(..)
    , MeetOrSlice(..)
    , SVGStyle(..)
    )
where

import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AT
import Data.Char (isAlpha, isHexDigit, digitToInt)
import Data.Colour.Names (readColourName)
import Data.Colour.SRGB
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing, maybeToList, catMaybes)
import qualified Data.Text as T
import Data.Text(Text(..), pack, unpack, empty, cons, snoc, append)
import Data.Tuple.Select
import Diagrams.Attributes
import Diagrams.Path
import Diagrams.Segment
import Diagrams.TwoD.Types
import Diagrams.Prelude
import Diagrams.SVG.Path
import Diagrams.SVG.Tree
import Text.CSS.Parse
import qualified Data.HashMap.Strict as H
import Data.Colour
import Data.Colour.RGBSpace.HSL (hsl)
import Data.Word (Word8)

---------------------------------------------------

data CoreAttributes =
   CA { id1      :: Maybe Text
      , xmlbase  :: Maybe Text
      , xmllang  :: Maybe Text
      , xmlspace :: Maybe Text
      }

data ConditionalProcessingAttributes =
  CPA { requiredFeatures   :: Maybe Text
      , requiredExtensions :: Maybe Text
      , systemLanguage     :: Maybe Text
      }

data DocumentEventAttributes =
   DEA { onunload :: Maybe Text
       , onabort  :: Maybe Text
       , onerror  :: Maybe Text
       , onresize :: Maybe Text
       , onscroll :: Maybe Text
       , onzoom   :: Maybe Text
       }

data GraphicalEventAttributes =
   GEA { onfocusin   :: Maybe Text
       , onfocusout  :: Maybe Text
       , onactivate  :: Maybe Text
       , onclick     :: Maybe Text
       , onmousedown :: Maybe Text
       , onmouseup   :: Maybe Text
       , onmouseover :: Maybe Text
       , onmousemove :: Maybe Text
       , onmouseout  :: Maybe Text
       , onload      :: Maybe Text
       }

data XlinkAttributes =
   XLA { xlinkHref    :: Maybe Text
       , xlinkShow    :: Maybe Text
       , xlinkActuate :: Maybe Text
       , xlinkType    :: Maybe Text
       , xlinkRole    :: Maybe Text
       , xlinkArcrole :: Maybe Text
       , xlinkTitle   :: Maybe Text
       }

data FilterPrimitiveAttributes = 
   FPA { x      :: Maybe Text
       , y      :: Maybe Text
       , width  :: Maybe Text
       , height :: Maybe Text
       , result :: Maybe Text
       }

data NameSpaces =
   NSP { xlink    :: Maybe Text
       , dc       :: Maybe Text
       , cc       :: Maybe Text
       , rdf      :: Maybe Text
       , svg      :: Maybe Text
       , sodipodi :: Maybe Text
       , inkscape :: Maybe Text
       } deriving Show

--------------------------------------------------------------------------------
-- General parsing functions
--------------------------------------------------------------------------------

-- | Parsing content separated by something, e.g. ";"  like in: "a;b;c;d;" or "a;b;c;d"
separatedBy parse sep = do ls <- many1 (choice [parseOne parse sep, parseOne' parse])
                           return ls

parseOne parse sep = do AT.skipSpace
                        s <- parse
                        AT.string sep
                        return s

parseOne' parse = do AT.skipSpace
                     s <- parse
                     return s

-- | See http://www.haskell.org/haskellwiki/Compose
compose :: [a -> a] -> a -> a
compose fs v = Prelude.foldl (flip (.)) id fs $ v

parseDouble :: Text -> Double
parseDouble l = either (const 0) id (AT.parseOnly AT.double l)

parseMaybeDouble :: Maybe Text -> Double
parseMaybeDouble l | isJust l = either (const 0) id (AT.parseOnly AT.double (fromJust l))
                   | otherwise = 0

parseToDouble :: Maybe Text -> Maybe Double
parseToDouble l | isJust l = either (const Nothing) Just (AT.parseOnly AT.double (fromJust l))
                | otherwise = Nothing

pp = parseDouble . pack


parsePoints :: Text -> [(Double, Double)]
parsePoints t = either (const []) id (AT.parseOnly (many' parsePoint) t)

parsePoint =
   do AT.skipSpace
      a <- double
      AT.char ','
      b <- double
      return (a,b)

parseUntil c = AT.manyTill AT.anyChar (AT.char c)

data Tup = TS1 Text | TS2 Text Text | TS3 Text Text Text 
         | T1  Double | T2  Double Double | T3  Double Double Double 
         deriving (Show)

parse1 =
  do AT.skipSpace
     AT.char '('
     a <- parseUntil ')'
     return (TS1 (pack a))

parse2 =
  do AT.skipSpace
     AT.char '('
     a <- parseUntil ','
     b <- parseUntil ')'
     return (TS2 (pack a) (pack b))

parse3 =
  do AT.skipSpace
     AT.char '('
     a <- parseUntil ','
     b <- parseUntil ','
     c <- parseUntil ')'
     return (TS3 (pack a) (pack b) (pack c))

-----------------------------------------------------------------------------------------------------------------
-- Transformations, see http://www.w3.org/TR/SVG11/coords.html#TransformAttribute
--    
-- Example:  transform="translate(-121.1511,-167.6958) matrix(4.675013,0,0,4.675013,-1353.75,-678.4329)"
-----------------------------------------------------------------------------------------------------------------

data Transform = Tr Tup
               | Matrix Double Double Double Double Double Double
               | Rotate Tup 
               | Scale Tup
               | SkewX Tup
               | SkewY Tup  deriving (Show)

parseTr :: Maybe Text -> [Transform]
parseTr =  catMaybes .
           (either (const []) id) .
           ( AT.parseOnly (separatedBy parseTransform " ") ).
           (fromMaybe empty)

parseTransform = AT.choice [matr, trans, scle, rot, skewX, skewY]

applyTr trs = compose (map getTransformations trs)

getTransformations (Tr (T1 x))   = translateX x
getTransformations (Tr (T2 x y)) = (translateX x) . (translateY y)

-- | See http://www.w3.org/TR/SVG11/coords.html#TransformMatrixDefined
getTransformations (Matrix a b c d e f)
   = (translateX x) . (translateY y) . (scaleX scX) . (scaleY scY) . (rotateBy angle)
  where (angle, scX, scY, x, y) = matrixDecompose (Matrix a b c d e f)

getTransformations (Rotate (T1 angle)) = rotateBy angle
-- getTransformations (Rotate (T3 angle x y)) = rotateAbout (p2 (x,y)) (angle)
getTransformations (Scale (T1 x))   = scaleX x
getTransformations (Scale (T2 x y)) = (scaleX x) . (scaleY y)
getTransformations (SkewX (T1 x)) = id
getTransformations (SkewY (T1 y)) = id

-- | See http://math.stackexchange.com/questions/13150/extracting-rotation-scale-values-from-2d-transformation-matrix/13165#13165
matrixDecompose (Matrix m11 m12 m21 m22 m31 m32) = (rotation, scX, scY, transX, transY)
  where
    rotation = atan2 m12 m21
    scX | m11 >= 0  =   sqrt (m11*m11 + m21*m21)
        | otherwise = - sqrt (m11*m11 + m21*m21)
    scY | m22 >= 0  =   sqrt (m12*m12 + m22*m22)
        | otherwise = - sqrt (m12*m12 + m22*m22)
    (transX, transY) = (m31, m32)

matr =
   do AT.skipSpace
      AT.string "matrix"
      AT.skipSpace
      AT.char '('
      a <- parseUntil ','
      b <- parseUntil ','
      c <- parseUntil ','
      d <- parseUntil ','
      e <- parseUntil ','
      f <- parseUntil ')'
      return (Just $ Matrix (pp a) (pp b) (pp c) (pp d) (pp e) (pp f) )

evalTup (TS1 x)     = T1 (parseDouble x)
evalTup (TS2 x y)   = T2 (parseDouble x) (parseDouble y)
evalTup (TS3 x y z) = T3 (parseDouble x) (parseDouble y) (parseDouble z)

trans =
  do AT.skipSpace
     AT.string "translate" 
     tup <- AT.choice [parse2, parse1]
     return (Just $ Tr (evalTup tup))

scle =
  do AT.skipSpace
     AT.string "scale"
     tup <- AT.choice [parse2, parse1]
     return (Just $ Scale (evalTup tup))

rot =
  do AT.skipSpace
     AT.string "rotate"
     tup <- AT.choice [parse3, parse1]
     return (Just $ Rotate (evalTup tup))

skewX =
  do AT.skipSpace
     AT.string "skewX"
     angle <- parse1
     return (Just $ SkewX (evalTup angle))

skewY =
  do AT.skipSpace
     AT.string "skewY"
     angle <- parse1
     return (Just $ SkewY (evalTup angle))

------------------------------------------------------------------------------------------------
-- Parse the styles of various presentation attributes.
-- Example: <path fill="#FFFFFF" ...
-- Alternative way to writing everything into style="
------------------------------------------------------------------------------------------------

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
      , fontSize :: Maybe Text
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
      }

-- parsePA :: PresentationAttributes -> HashMaps > [(SVGStyle a)]
parsePA pa (nodes,_,grad) = l
  where l = catMaybes
         [(parseTempl (styleFillVal grad))      (fill pa),
          (parseTempl styleFillRuleVal)         (fillRuleSVG pa),
          (parseTempl styleFillOpacityVal)      (fillOpacity pa),
          (parseTempl (styleStrokeVal grad))    (strokeSVG pa),
          (parseTempl styleStrokeWidth)         (strokeWidth pa),
          (parseTempl styleStrokeLineCapVal)    (strokeLinecap pa),
          (parseTempl styleStrokeLineJoinVal)   (strokeLinejoin pa),
          (parseTempl styleStrokeMiterLimitVal) (strokeMiterlimit pa),
          (parseTempl (styleClipPathVal nodes)) (clipPath pa),
          (parseTempl styleStrokeDashArrayVal)  (strokeDasharray pa) ]

--------------------------------------------------------------------------------------------
-- Parse the style attribute, see http://www.w3.org/TR/SVG/painting.html
--                            and http://www.w3.org/TR/SVG/styling.html
-- Example: style="fill:white;stroke:black;stroke-width:0.503546"
--------------------------------------------------------------------------------------------

data SVGStyle a = Fill (AlphaColour a) | FillTex Texture | FillOpacity Double | FillRule FR
                | Stroke (AlphaColour a) | StrokeTex Texture | StrokeWidth LenPercent | StrokeLineCap LineCap
                | StrokeLineJoin LineJoin | StrokeMiterLimit Double | StrokeDasharray [LenPercent] | StrokeOpacity Double
                | ClipPath (Path R2)
                | EmptyStyle

data Unit = EM | EX | PX | IN | CM | MM | PT | PC deriving (Show)
data FR = Even_Odd | Nonzero | Inherit  deriving (Show)
data LenPercent = Len Double | Percent Double -- TODO implement

instance Show (SVGStyle a) where
  show (Fill c) = "Fill"
  show (FillTex t) = "Filltex"
  show (FillOpacity d) = "FillOpacity"
  show (FillRule r) = "FillRule"
  show (Stroke s) = "Stroke"
  show (StrokeTex s) = "StrokeTex"
  show (StrokeWidth w) = "StrokeWidth"
  show (StrokeLineCap l) = "StrokeLineCap"
  show (StrokeLineJoin l) = "StrokeLineJoin"
  show (StrokeMiterLimit l) = "StrokeMiterLimit"
  show (StrokeDasharray l) = "StrokeDasharray"
  show (StrokeOpacity o) = "StrokeOpacity"
  show (ClipPath path) = "ClipPath"
  show (EmptyStyle) = ""

instance Show (LenPercent) where
  show (Len x) = show x
  show (Percent x) = show x

parseStyles :: (Read a, Floating a, RealFrac a, Ord a) => Maybe Text -> HashMaps -> [(SVGStyle a)]
parseStyles text hmaps = either (const []) id $
                         AT.parseOnly (separatedBy (parseStyleAttr hmaps) ";") (fromMaybe empty text)

parseStyleAttr (ns,css,grad) =
  AT.choice [styleFill grad, styleFillOpacity, styleFillRule, 
             styleStroke grad, styleStrokeWidth, styleStopColor, styleStopOpacity,
             styleStrokeLineCap, styleStrokeLineJoin, styleStrokeMiterLimit, styleClipPath ns, styleStrokeDashArray]

-- | This function is called on every tag and returns a list of style-attributes to apply (if there is a rule that matches)
-- TO DO: CSS2 + CSS3 selectors
cssStylesFromMap :: (Read a, Floating a, RealFrac a, Ord a) =>
                    HashMaps -> Text -> Maybe Text ->  Maybe Text -> [(SVGStyle a)]
cssStylesFromMap (ns,css,grad) tagName id_ class_ = parseStyles ( Just ( T.concat ( map f attributes ) ) ) (ns,css,grad)
  where f (attr, val) = (attr `snoc` ':') `append` (val `snoc` ';')
        styleFromClass cl = [H.lookup ('.' `cons` cl) css] ++ [H.lookup (tagName `append` ('.' `cons` cl)) css]
        attributes = concat $ catMaybes
                   ( [H.lookup "*" css] ++    -- apply this style to every element
                     (if isJust id_ then [H.lookup ('#' `cons` (fromJust id_)) css] else []) ++
                     (concat (map styleFromClass (if isJust class_ then T.words $ fromJust class_ else [])))
                   )

-- parseTempl :: (Floating a, Ord a) => Parser a -> Maybe Text -> Maybe a
parseTempl p = (either (const Nothing) Just) .
               (AT.parseOnly p).
               (fromMaybe empty)

p x = unL $ fromMaybe (Len 0) $ parseTempl styleLength x -- TODO implement percentage (relative size to viewbox)
  where unL (Len x) = x
        unL _ = 1

parseIRI = do AT.choice [ funcIRI, absoluteOrRelativeIRI ]

funcIRI =
  do AT.skipSpace
     AT.string "url("
     absrel <- parseUntil '#'
     fragment <- parseUntil ')'
     return (T.pack absrel, T.pack fragment)

absoluteOrRelativeIRI =
  do AT.skipSpace
     absrel <- parseUntil '#'
     fragment <- takeText
     return (T.pack absrel, fragment)

fragment x = fromMaybe T.empty $ fmap snd (parseTempl parseIRI x) -- look only for the text after "#"

applyStyleSVG stylesFromMap hmap = compose (map getStyles (stylesFromMap hmap))

-- | Inital styles, see: http://www.w3.org/TR/SVG/painting.html#FillProperty
initialStyles = lwL 1 . fc black . lineCap LineCapButt . lineJoin LineJoinMiter -- lineMiterLimit 4
               -- fillRule nonzero -- TODO
               -- fillOpcacity 1 -- TODO
               -- stroke none -- TODO
               -- stroke-dasharray none
               -- stroke-dashoffset 0 #
               -- stroke-opacity 1 #
               -- display inline

getStyles (Fill c) = fcA c
getStyles (FillTex x) = fillTexture x
getStyles (FillOpacity d) = id -- we currently don't differentiate between fill opacity and stroke opacity
getStyles (FillRule Even_Odd) = fillRule EvenOdd
getStyles (FillRule Nonzero) = id
getStyles (FillRule Inherit) = id
getStyles (Stroke x) = lcA x
getStyles (StrokeTex x) = lineTexture x
getStyles (StrokeWidth (Len x)) = lwL x
getStyles (StrokeWidth (Percent x)) = lwG x
getStyles (StrokeLineCap x) = lineCap x
getStyles (StrokeLineJoin x) = lineJoin x
getStyles (StrokeMiterLimit x) = id
getStyles (StrokeDasharray array) = dashingL (map dash array) 0
  where dash (Len x) = x
        dash (Percent x) = x -- TODO implement percent length
getStyles (StrokeOpacity x) = id -- we currently don't differentiate between fill opacity and stroke opacity
getStyles (ClipPath path) = clipBy path
getStyles _ = id

-- | Example: style="fill:#ffb13b" style="fill:red"
styleFill hmap =
  do AT.skipSpace
     AT.string "fill:"
     AT.skipSpace
     styleFillVal hmap

styleFillVal gradients = AT.choice [ styleFillColourVal, styleFillTexURL gradients ]

styleFillColourVal =
  do c <- AT.choice [colorRRGGBB, colorRGB, colorString, colorRGBPercent, colorHSLPercent, colorNone]
     return (Fill c)

styleFillTexURL gradients =
  do (absrel,fragment) <- parseIRI
     let t = H.lookup fragment gradients
     if isJust t then return (FillTex (fromJust t))
                 else return EmptyStyle

-- | Example: style="fill-rule:evenodd"
styleFillRule =
  do AT.skipSpace
     AT.string "fill-rule:"
     AT.skipSpace
     styleFillRuleVal

styleFillRuleVal =
  do AT.choice [ (do{ AT.string "evenodd"; return $ FillRule Even_Odd }),
                 (do{ AT.string "nonzero"; return $ FillRule Nonzero }),
                 (do{ AT.string "inherit"; return $ FillRule Inherit })
               ]

-- | Example: style="fill:#ffb13b" style="fill:red"
styleFillOpacity =
  do AT.skipSpace
     AT.string "fill-opacity:"
     AT.skipSpace
     styleFillOpacityVal

styleFillOpacityVal =
  do o <- double
     return (FillOpacity o)

-- | Example: style="stroke:black"
styleStroke hmap =
  do AT.skipSpace
     AT.string "stroke:"
     AT.skipSpace
     styleStrokeVal hmap

styleStrokeVal gradients = AT.choice [ styleStrokeColourVal, styleStrokeTexURL gradients ]

styleStrokeColourVal =
  do c <- AT.choice [colorRRGGBB, colorRGB, colorString, colorRGBPercent, colorHSLPercent, colorNone]
     return (Stroke c)

styleStrokeTexURL gradients =
  do (absrel,fragment) <- parseIRI
     let t = H.lookup fragment gradients
     if isJust t then return (StrokeTex (fromJust t))
                 else return EmptyStyle

-- | Example: style="stroke-width:0.503546"
styleStrokeWidth =
  do AT.skipSpace
     AT.string "stroke-width:"
     len <- styleLength
     return (StrokeWidth len)

styleLength = do AT.skipSpace
                 d <- AT.double
                 AT.skipSpace
                 AT.choice [ styleLengthWithUnit d, lengthPercent d, return (Len d) ]

styleLengthWithUnit d =
  do u <- styleUnit
     return (Len (d * (unitFactor u)))

lengthPercent d =
  do AT.string "%"
     return (Percent d)

styleUnit = do AT.choice [styleEM,styleEX,stylePX,styleIN,styleCM,styleMM,stylePT,stylePC]

styleEM = do { AT.choice [AT.string "em", AT.string "EM"]; return EM }
styleEX = do { AT.choice [AT.string "ex", AT.string "EX"]; return EX }
stylePX = do { AT.choice [AT.string "px", AT.string "PX"]; return PX }
styleIN = do { AT.choice [AT.string "in", AT.string "IN"]; return IN }
styleCM = do { AT.choice [AT.string "cm", AT.string "CM"]; return CM }
styleMM = do { AT.choice [AT.string "mm", AT.string "MM"]; return MM }
stylePT = do { AT.choice [AT.string "pt", AT.string "PT"]; return PT }
stylePC = do { AT.choice [AT.string "pc", AT.string "PC"]; return PC }

unitFactor EM = 1
unitFactor EX = 1
unitFactor PX = 1
unitFactor IN = 90
unitFactor CM = 35.43307
unitFactor MM = 3.543307
unitFactor PT = 1.25
unitFactor PC = 15

-- | Example: "stroke-linecap:butt"
styleStrokeLineCap =
  do AT.skipSpace
     AT.string "stroke-linecap:"
     AT.skipSpace
     styleStrokeLineCapVal

styleStrokeLineCapVal =
  do lc <- AT.choice [butt,round0,square0]
     return (StrokeLineCap lc)

butt    = do { AT.string "butt";   return LineCapButt }
round0  = do { AT.string "round";  return LineCapRound }
square0 = do { AT.string "square"; return LineCapSquare }

-- | Example: "stroke-linejoin:miter;"
styleStrokeLineJoin =
  do AT.skipSpace
     AT.string "stroke-linejoin:"
     AT.skipSpace
     styleStrokeLineJoinVal

styleStrokeLineJoinVal =
  do lj <- AT.choice [miter,round1,bevel]
     return (StrokeLineJoin lj)

miter  = do { AT.string "miter"; return LineJoinMiter }
round1 = do { AT.string "round"; return LineJoinRound }
bevel  = do { AT.string "bevel"; return LineJoinBevel }

styleClipPath hmap =
  do AT.skipSpace
     AT.string "clip-path:"
     AT.skipSpace
     styleClipPathVal hmap

styleClipPathVal hmap =
  do (absrel,fragment) <- parseIRI
     let t = H.lookup fragment hmap
     if isJust t then return (ClipPath $ evalPath hmap (fromJust t))
                 else return EmptyStyle

-- | Evaluate the tree to a path that is needed for clipPaths
evalPath :: H.HashMap Text Tag -> Tag -> Path R2
evalPath hmap (Leaf             id1 path diagram) = path
evalPath hmap (Reference selfId id1 wh f) = evalPath hmap (lookUp hmap (fragment id1))
evalPath hmap (SubTree _        id1 viewBox ar f children) = mconcat (map (evalPath hmap) children)
evalPath hmap _ = mempty

-- | Lookup a diagram and return an empty diagram in case the SVG-file has a wrong reference
lookUp hmap i | isJust l  = fromJust l
              | otherwise = Leaf Nothing mempty mempty -- an empty diagram if we can't find the id
  where l = H.lookup i hmap

-- | Example: "stroke-miterlimit:miter;"
styleStrokeMiterLimit =
  do AT.skipSpace
     AT.string "stroke-miterlimit:"
     AT.skipSpace
     styleStrokeMiterLimitVal

styleStrokeMiterLimitVal =
  do l <- double
     return (StrokeMiterLimit l)

styleStrokeDashArray =
  do AT.skipSpace
     AT.string "stroke-dasharray:"
     styleStrokeDashArrayVal

styleStrokeDashArrayVal =
  do len <- parseLengths
     return (StrokeDasharray len)

parseLengths = separatedBy styleLength ","

styleStrokeOpacity =
  do AT.skipSpace
     AT.string "stroke-opacity:"
     AT.skipSpace
     styleOpacityVal

styleOpacityVal =
  do l <- double
     return (FillOpacity l)

styleStopColor =
  do AT.skipSpace
     AT.string "stop-color:"
     AT.skipSpace
     styleFillColourVal

styleStopOpacity =
  do AT.skipSpace
     AT.string "stop-opacity:"
     AT.skipSpace
     styleOpacityVal

-- To Do: Visibility, marker

-----------------------------------------------------------------------
-- Colors, see http://www.w3.org/TR/SVG/color.html and 
--             http://www.w3.org/TR/SVG/painting.html#SpecifyingPaint
-----------------------------------------------------------------------

colorString =
  do a <- Data.Attoparsec.Text.takeWhile isAlpha
     c <- readColourName (unpack a)
     return (opaque c)

colorRGB =
  do AT.char '#'
     h0 <- satisfy isHexDigit
     h1 <- satisfy isHexDigit
     h2 <- satisfy isHexDigit
     return $ opaque ( sRGB24 (fromIntegral ((digitToInt h0) * 16))
                              (fromIntegral ((digitToInt h1) * 16))
                              (fromIntegral ((digitToInt h2) * 16)) )

colorRRGGBB =
  do AT.char '#'
     h0 <- satisfy isHexDigit
     h1 <- satisfy isHexDigit
     h2 <- satisfy isHexDigit
     h3 <- satisfy isHexDigit
     h4 <- satisfy isHexDigit
     h5 <- satisfy isHexDigit
     return $ opaque ( sRGB24 (fromIntegral ((digitToInt h0) * 16 + (digitToInt h1)) )
                              (fromIntegral ((digitToInt h2) * 16 + (digitToInt h3)) )
                              (fromIntegral ((digitToInt h4) * 16 + (digitToInt h5)) ) )

colorRGBPercent =
  do AT.string "rgb"
     AT.skipSpace
     AT.char '('
     r <- parseUntil '%'
     AT.skipSpace
     AT.char ','
     g <- parseUntil '%'
     AT.skipSpace
     AT.char ','
     b <- parseUntil '%'
     AT.skipSpace
     AT.char ')'
     return $ opaque (sRGB (read r) (read g) (read b))

colorHSLPercent =
  do AT.string "hsl"
     AT.skipSpace
     AT.char '('
     h <- parseUntil ','
     s <- parseUntil '%'
     AT.skipSpace
     AT.char ','
     l <- parseUntil '%'
     AT.skipSpace
     AT.char ')'
     let c = hsl (read h) (read s) (read l)
     return $ opaque (sRGB (channelRed c) (channelGreen c) (channelBlue c))

colorNone =
  do AT.string "none"
     return transparent


-------------------------------------------------------------------------------------
-- | Example: viewBox="0 0 100 30"
parseViewBox x = parseTempl viewBox x

viewBox =
  do AT.skipSpace
     minx <- double
     AT.skipSpace
     miny <- double
     AT.skipSpace
     width  <- double
     AT.skipSpace
     height <- double
     AT.skipSpace
     return (minx, miny, width, height)

-------------------------------------------------------------------------------------
-- Parse preserve aspect ratio
-- e.g. preserveAspectRatio="xMaxYMax meet"
-------------------------------------------------------------------------------------

parsePreserveAR x = parseTempl preserveAR x

preserveAR =
   do AT.skipSpace
      align <- AT.choice [alignXMinYMin,alignXMidYMin,alignXMaxYMin,alignXMinYMid,alignXMidYMid,
                          alignXMaxYMid,alignXMinYMax,alignXMidYMax,alignXMaxYMax]
      AT.skipSpace
      meetOrSlice <- AT.choice [meet, slice]
      return (PAR align meetOrSlice)

meet =
   do AT.string "meet"
      return Meet

slice =
   do AT.string "slice"
      return Slice

alignXMinYMin =
   do AT.string "xMinYMin"
      return (AlignXY 0 0)

alignXMidYMin =
   do AT.string "xMidYMin"
      return (AlignXY 0.5 0)

alignXMaxYMin =
   do AT.string "xMaxYMin"
      return (AlignXY 1 0)

alignXMinYMid =
   do AT.string "xMinYMid"
      return (AlignXY 0 0.5)

alignXMidYMid =
   do AT.string "xMidYMid"
      return (AlignXY 0.5 0.5)

alignXMaxYMid =
   do AT.string "xMaxYMid"
      return (AlignXY 1 0.5)

alignXMinYMax =
   do AT.string "xMinYMax"
      return (AlignXY 0 1)

alignXMidYMax =
   do AT.string "xMidYMax"
      return (AlignXY 0.5 1)

alignXMaxYMax =
   do AT.string "xMaxYMax"
      return (AlignXY 1 1)

