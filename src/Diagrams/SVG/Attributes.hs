{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, TypeFamilies, FlexibleContexts #-}

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
    , parseToDouble
    , parsePoints
    , parseTempl
    , parseIRI
    -- * Transformations
    , applyTr
    , parseTr
    -- * Parsing the style attribute
    , applyStyleSVG
    , parseStyles
    , parseLengths
    , parseViewBox
    , parsePA
    , cssStylesFromMap
    , fragment
    , p
    , parseSpread
    -- * Parsing Colors
    -- * Parsing preserve aspect ratio
    , parsePreserveAR
    , PreserveAR(..)
    , AlignSVG(..)
    , Place(..)
    , MeetOrSlice(..)
    , SVGStyle(..)
    , PresentationAttributes(..)
    )
where

import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AT
import           Data.Char (isAlpha, isHexDigit, digitToInt)
import           Data.Colour
import           Data.Colour.Names (readColourName)
import           Data.Colour.SRGB
import           Data.Colour.RGBSpace.HSL (hsl)
import qualified Data.HashMap.Strict as H
import           Data.Maybe (fromMaybe, fromJust, isJust, isNothing, maybeToList, catMaybes)
import qualified Data.Text as T
import           Data.Text(Text(..), pack, unpack, empty, cons, snoc, append)
import           Data.Typeable
import           Data.Word (Word8)
import           Diagrams.Prelude hiding (fillOpacity, strokeOpacity)
import           Diagrams.SVG.Path
import           Diagrams.SVG.Tree
import           Text.CSS.Parse
import           Diagrams.Core.Transform
import           Data.Digits

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

-- | See <http://www.haskell.org/haskellwiki/Compose>
compose :: [a -> a] -> a -> a
compose fs v = Prelude.foldl (flip (.)) id fs $ v

parseDouble :: RealFloat n => Text -> n
parseDouble l = either (const 0) (fromRational . toRational) (AT.parseOnly myDouble l)

parseToDouble :: RealFloat n => Maybe Text -> Maybe n
parseToDouble l | isJust l = either (const Nothing) (Just . fromRational . toRational) (AT.parseOnly myDouble (fromJust l))
                | otherwise = Nothing
pp = parseDouble . pack

parsePoints :: RealFloat n => Text -> [(n, n)]
parsePoints t = either (const []) id (AT.parseOnly (many' parsePoint) t)

parsePoint :: RealFloat n => Parser (n, n)
parsePoint =
   do AT.skipSpace
      a <- double
      AT.char ','
      b <- double
      return ( (fromRational . toRational) a, (fromRational . toRational) b)

parseUntil c = AT.manyTill AT.anyChar (AT.char c)

data Tup n = TS1 Text | TS2 Text Text | TS3 Text Text Text 
         | T1  n | T2  n n | T3 n n n 
         deriving Show

parse1 =
  do AT.skipSpace
     AT.char '('
     a <- AT.takeTill (== ')')
     AT.char ')'
     return (TS1 a)

parse2 =
  do AT.skipSpace
     AT.char '('
     a <- AT.takeTill (\c -> c == ',' || c == ' ')
     AT.choice [AT.char ',', AT.char ' ']
     AT.skipSpace
     b <- AT.takeTill (== ')')
     AT.char ')'
     return (TS2 a b)

parse3 =
  do AT.skipSpace
     AT.char '('
     a <- AT.takeTill (\c -> c == ',' || c == ' ')
     AT.choice [AT.char ',', AT.char ' ']
     b <- AT.takeTill (\c -> c == ',' || c == ' ')
     AT.choice [AT.char ',', AT.char ' ']
     c <- AT.takeTill (== ')')
     AT.char ')'
     return (TS3 a b c)

-----------------------------------------------------------------------------------------------------------------
-- Transformations, see <http://www.w3.org/TR/SVG11/coords.html#TransformAttribute>
--    
-- Example: transform="translate(-121.1511,-167.6958) matrix(4.675013,0,0,4.675013,-1353.75,-678.4329)"
-----------------------------------------------------------------------------------------------------------------

data Transform n = Tr (Tup n)
               | Matrix n n n n n n
               | Rotate (Tup n)
               | Scale (Tup n)
               | SkewX (Tup n)
               | SkewY (Tup n) deriving Show

parseTr :: RealFloat n => Maybe Text -> [Transform n]
parseTr =  reverse .
           catMaybes .
           (either (const []) id) .
           ( AT.parseOnly (AT.many1 parseTransform)) .
           (fromMaybe empty)

parseTransform = AT.choice [matr, trans, scle, rot, skewX, skewY]

applyTr trs = compose (map getTransformations trs)

getTransformations (Tr (T1 x))   =  translateX x
getTransformations (Tr (T2 x y)) = (translateX x) . (translateY y)

-- | See <http://www.w3.org/TR/SVG11/coords.html#TransformMatrixDefined>
getTransformations (Matrix a b c d e f)
   = (translateX x) . (translateY y) . (rotateBy angle) . (scaleX scX) . (scaleY scY)
  where (angle, scX, scY, x, y) = matrixDecompose (Matrix a b c d e f)


-- matrix(0.70710678,-0.70710678,0.70710678,0.70710678,0,0)

getTransformations (Rotate (T1 angle)) = rotateBy angle
getTransformations (Rotate (T3 angle x y)) = id -- rotationAbout (p2 (x,y)) (angle)
getTransformations (Scale (T1 x))   = scaleX x
getTransformations (Scale (T2 x y)) = (scaleX x) . (scaleY y)
getTransformations (SkewX (T1 x)) = id
getTransformations (SkewY (T1 y)) = id

-- | See <http://math.stackexchange.com/questions/13150/extracting-rotation-scale-values-from-2d-transformation-matrix/13165#13165>
matrixDecompose (Matrix m11 m12 m21 m22 m31 m32) = (rotation, scX, scY, transX, transY)
  where
    rotation = (atan2 m12 m22) / (2*pi)
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
     tup <- AT.choice [parse1, parse3]
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

parsePA :: (RealFloat n, RealFloat a, Read a) => PresentationAttributes -> HashMaps b n -> [(SVGStyle n a)]
parsePA pa (nodes,css,grad) = l
  where l = catMaybes
         [(parseTempl (styleFillVal css grad))   (fill pa),
          (parseTempl styleFillRuleVal)          (fillRuleSVG pa),
          (parseTempl styleFillOpacityVal)       (fillOpacity pa),
          (parseTempl styleOpacityVal)           (Diagrams.SVG.Tree.opacity pa),
          (parseTempl styleStrokeOpacityVal)     (strokeOpacity pa),
          (parseTempl (styleStrokeVal css grad)) (strokeSVG pa),
          (parseTempl styleStrokeWidthVal)       (strokeWidth pa),
          (parseTempl styleStrokeLineCapVal)     (strokeLinecap pa),
          (parseTempl styleStrokeLineJoinVal)    (strokeLinejoin pa),
          (parseTempl styleStrokeMiterLimitVal)  (strokeMiterlimit pa),
          (parseTempl styleFontFamily)           (fontFamily pa),
          (parseTempl styleFontSize)             (fntSize pa),
          (parseTempl (styleClipPathVal nodes))  (clipPath pa),
          (parseTempl styleStrokeDashArrayVal)   (strokeDasharray pa) ]

--------------------------------------------------------------------------------------------
-- Parse the style attribute, see <http://www.w3.org/TR/SVG/painting.html>
--                            and <http://www.w3.org/TR/SVG/styling.html>
-- Example: style="fill:white;stroke:black;stroke-width:0.503546"
--------------------------------------------------------------------------------------------

data SVGStyle n a = Fill (AlphaColour a) | FillTex (Texture n) | FillOpacity Double | FillRule FR | Opacity Double
                  | Stroke (AlphaColour a) | StrokeTex (Texture n) | StrokeWidth (LenPercent n) | StrokeLineCap LineCap
                  | StrokeLineJoin LineJoin | StrokeMiterLimit n | StrokeDasharray [LenPercent n] | StrokeOpacity Double
                  | FontFamily String | FontStyle FStyle | FontVariant FVariant | FontWeight FWeight | FontStretch FStretch
                  | FontSize (LenPercent n)
                  | ClipPath (Path V2 n)
                  | EmptyStyle


-- "font-style:normal;text-align:start;letter-spacing:0px;word-spacing:0px;writing-mode:lr-tb;text-anchor:start"
-- fontStyle letterSpacing wordSpacing writingMode textAnchor

data Unit = EM | EX | PX | IN | CM | MM | PT | PC deriving Show
data FR = Even_Odd | Nonzero | Inherit  deriving Show
data LenPercent n = Len n | Percent n

instance Show (SVGStyle n a) where
  show (Fill c) = "Fill"
  show (FillTex t) = "Filltex"
  show (FillRule r) = "FillRule"
  show (FillOpacity d) = "FillOpacity"
  show (FontFamily f) = "FontFamily"
  show (FontStyle f) = "FontStyle"
  show (FontVariant f) = "FontVariant"
  show (FontWeight f) = "FontWeight"
  show (FontStretch f) = "FontStretch"
  show (FontSize f) = "FontSize"
  show (Diagrams.SVG.Attributes.Opacity d) = "Opacity"
  show (StrokeOpacity o) = "StrokeOpacity"
  show (Stroke s) = "Stroke"
  show (StrokeTex s) = "StrokeTex"
  show (StrokeWidth w) = "StrokeWidth"
  show (StrokeLineCap l) = "StrokeLineCap"
  show (StrokeLineJoin l) = "StrokeLineJoin"
  show (StrokeMiterLimit l) = "StrokeMiterLimit"
  show (StrokeDasharray l) = "StrokeDasharray"
  show (ClipPath path) = "ClipPath"
  show (EmptyStyle) = ""

instance Show (LenPercent n) where
  show (Len x) = "" -- show x
  show (Percent x) = "" -- show x

-- parseStyles :: (Read a, RealFloat a, RealFloat n) => Maybe Text -> HashMaps b n -> [(SVGStyle n a)]
parseStyles text hmaps = either (const []) id $
                         AT.parseOnly (separatedBy (parseStyleAttr hmaps) ";") (fromMaybe empty text)

-- parseStyleAttr :: (Read a, RealFloat a, RealFloat n) => HashMaps b n -> Parser (SVGStyle n a)
parseStyleAttr (ns,css,grad) =
  AT.choice [styleFillRule, styleStrokeWidth, styleStrokeDashArray, styleFill css grad, styleStroke css grad, styleStopColor,
             styleStopOpacity, styleFillOpacity, styleStrokeOpacity, styleOpacity,
             styleFontFamily, styleFontStyle, styleFontVariant, styleFontWeight, styleFontStretch, styleFontSize,
             styleStrokeLineCap, styleStrokeLineJoin, styleStrokeMiterLimit, styleClipPath ns, skipOne]

skipOne = do str <- AT.manyTill AT.anyChar (AT.char ';') -- TODO end of input ?
             return EmptyStyle

-- | This function is called on every tag and returns a list of style-attributes to apply 
--   (if there is a rule that matches)
-- TO DO: CSS2 + CSS3 selectors
-- cssStylesFromMap :: (Read a, RealFloat a, RealFloat n) =>
--                    HashMaps b n -> Text -> Maybe Text ->  Maybe Text -> [(SVGStyle n a)]
cssStylesFromMap (ns,css,grad) tagName id_ class_ = parseStyles ( Just ( T.concat ( map f attributes ) ) ) (ns,css,grad)
  where f (attr, val) = (attr `Data.Text.snoc` ':') `append` (val `Data.Text.snoc` ';')
        styleFromClass cl = [H.lookup ('.' `Data.Text.cons` cl) css] ++ [H.lookup (tagName `append` ('.' `Data.Text.cons` cl)) css]
        attributes = concat $ catMaybes
                   ( [H.lookup "*" css] ++    -- apply this style to every element
                     (if isJust id_ then [H.lookup ('#' `Data.Text.cons` (fromJust id_)) css] else []) ++
                     (concat (map styleFromClass (if isJust class_ then T.words $ fromJust class_ else [])))
                   )

-- | a template that deals with the common parser errors
parseTempl :: Parser a -> Maybe Text -> Maybe a
parseTempl p = (either (const Nothing) Just) .
               (AT.parseOnly p).
               (fromMaybe empty)

-- | Given a minimum and maximum value of a viewbox (x or y-direction) and a maybe a Text value
--   Parse this Text value as a length (with a unit) or a percentage relative to the viewbox (minx,maxx)
--   If parsers fails return def
p :: RealFloat n => (n,n) -> n -> Maybe Text -> n
p (minx,maxx) def x = unL $ fromMaybe (Len def) $ parseTempl styleLength x
  where unL (Len x) = x
        unL (Percent x) = x/100 * (maxx-minx)

parseIRI = do AT.choice [ funcIRI, absoluteOrRelativeIRI ]

funcIRI =
  do AT.skipSpace
     AT.string "url("
     absrel <- parseUntil '#'
     frag <- parseUntil ')'
     return (T.pack absrel, T.pack frag)

absoluteOrRelativeIRI =
  do AT.skipSpace
     absrel <- parseUntil '#'
     frag <- takeText
     return (T.pack absrel, frag)

fragment x = fmap snd (parseTempl parseIRI x) -- look only for the text after "#"

-- | Inital styles, see: <http://www.w3.org/TR/SVG/painting.html#FillProperty>
initialStyles = lwL 1 . fc black . lineCap LineCapButt . lineJoin LineJoinMiter . lineMiterLimit 4 . lcA transparent
                . fontSize medium
               -- fillRule nonzero -- TODO
               -- fillOpcacity 1 -- TODO
               -- stroke-opacity 1 #
               -- stroke-dasharray none
               -- stroke-dashoffset 0 #
               -- display inline

applyStyleSVG stylesFromMap hmap = compose (map getStyles (stylesFromMap hmap))

getStyles (Fill c) = fcA c
getStyles (FillTex x) = fillTexture x
getStyles (FillRule Even_Odd) = fillRule EvenOdd
getStyles (FillRule Nonzero) = id
getStyles (FillRule Inherit) = id
getStyles (FillOpacity x) = Diagrams.Prelude.opacity x
getStyles (FontFamily str) = font str
getStyles (FontStyle s) = id
getStyles (FontVariant s) = id
getStyles (FontWeight s) = id
getStyles (FontStretch s) = id
getStyles (FontSize (Len len)) = fontSize (local len)
-- getStyles (FontSize (Percent len)) = fontSize (local len)
getStyles (Diagrams.SVG.Attributes.Opacity x) = Diagrams.Prelude.opacity x
getStyles (StrokeOpacity x) | x == 0    = lwL 0
                            | otherwise = Diagrams.Prelude.opacity x -- we currently don't differentiate between fill opacity and stroke opacity
getStyles (Stroke x) = lcA x
getStyles (StrokeTex x) = lineTexture x
getStyles (StrokeWidth (Len x)) = lwL $ fromRational $ toRational x
getStyles (StrokeWidth (Percent x)) = lwG x
getStyles (StrokeLineCap x) = lineCap x
getStyles (StrokeLineJoin x) = lineJoin x
getStyles (StrokeMiterLimit x) = id
getStyles (StrokeDasharray array) = dashingL (map dash array) 0
   where dash (Len x) = x
         dash (Percent x) = x -- TODO implement percent length
getStyles (ClipPath path) = clipBy path
getStyles _ = id

-- | Example: style="fill:#ffb13b" style="fill:red"
styleFill css hmap =
  do AT.skipSpace
     AT.string "fill:"
     AT.skipSpace
     styleFillVal css hmap

styleFillVal css gradients = AT.choice [ styleFillColourVal, styleFillTexURL css gradients ]

styleFillColourVal =
  do c <- AT.choice [colorRRGGBB, colorRGB, colorString, colorRGBPercent, colorHSLPercent, colorNone, colorRGBWord]
     return (Fill c)

styleFillTexURL css gradients =
  do (absrel,frag) <- parseIRI
     let t = H.lookup frag gradients
     if isJust t then return (FillTex (getTexture (fromJust t)))
                 else return EmptyStyle
  where getTexture (Gr refId ga vb stops f) = f css ga (fromMaybe (0,0,0,0) vb) stops

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
  do o <- myDouble
     return (FillOpacity $ fromRational $ toRational o)

-- | Example: style="fill:#ffb13b" style="fill:red"
styleOpacity =
  do AT.skipSpace
     AT.string "opacity:"
     AT.skipSpace
     styleOpacityVal

styleOpacityVal =
  do o <- myDouble
     return (Diagrams.SVG.Attributes.Opacity $ fromRational $ toRational o)


-- | Example: style="stroke:black"
styleStroke css hmap =
  do AT.skipSpace
     AT.string "stroke:"
     AT.skipSpace
     styleStrokeVal css hmap

styleStrokeVal css gradients = AT.choice [ styleStrokeColourVal, styleStrokeTexURL css gradients ]

styleStrokeColourVal =
  do c <- AT.choice [colorRRGGBB, colorRGB, colorString, colorRGBPercent, colorHSLPercent, colorNone, colorRGBWord]
     return (Stroke c)

styleStrokeTexURL css gradients =
  do (absrel,frag) <- parseIRI
     let t = H.lookup frag gradients
     if isJust t then return (StrokeTex (getTexture (fromJust t)))
                 else return EmptyStyle
  where getTexture (Gr refId ga vb stops f) = f css ga (fromMaybe (0,0,0,0) vb) stops

-- | Example: style="stroke-width:0.503546"
styleStrokeWidth =
  do AT.skipSpace
     AT.string "stroke-width:"
     styleStrokeWidthVal

styleStrokeWidthVal =
  do len <- styleLength
     return (StrokeWidth len)

-------------------------------------------------------------------------------------
-- font

styleFontFamily =
  do AT.skipSpace
     AT.string "font-family:"
     str <- AT.manyTill AT.anyChar theEnd
     return (FontFamily str)

theEnd = do AT.choice [AT.char ';', do { endOfInput; return ' '}]


data FStyle = NormalStyle | Italic | Oblique | FSInherit

styleFontStyle =
  do AT.skipSpace
     AT.string "font-style:"
     AT.choice [ do { string "normal";  return (FontStyle NormalStyle)}
               , do { string "italic";  return (FontStyle Italic)}
               , do { string "oblique"; return (FontStyle Oblique)}
               , do { string "inherit"; return (FontStyle FSInherit)}
               ]


data FVariant = NormalVariant | SmallCaps | VInherit

styleFontVariant =
  do AT.skipSpace
     AT.string "font-variant:"
     AT.choice [ do { string "normal";      return (FontVariant NormalVariant)}
               , do { string "small-caps";  return (FontVariant SmallCaps)}
               , do { string "inherit";     return (FontVariant VInherit)}
               ]


data FWeight = NormalWeight | Bold | Bolder | Lighter 
             | N100 | N200 | N300 | N400 | N500 | N600 | N700 | N800 | N900
             | FWInherit

styleFontWeight =
  do AT.skipSpace
     AT.string "font-weight:"
     AT.choice [ do { string "normal";  return (FontWeight NormalWeight)}
               , do { string "bold";    return (FontWeight Bold)}
               , do { string "bolder";  return (FontWeight Bolder)}
               , do { string "lighter"; return (FontWeight Lighter)}
               , do { string "100";     return (FontWeight N100)}
               , do { string "200";     return (FontWeight N200)}
               , do { string "300";     return (FontWeight N300)}
               , do { string "400";     return (FontWeight N400)}
               , do { string "500";     return (FontWeight N500)}
               , do { string "600";     return (FontWeight N600)}
               , do { string "700";     return (FontWeight N700)}
               , do { string "800";     return (FontWeight N800)}
               , do { string "900";     return (FontWeight N900)}
               , do { string "inherit"; return (FontWeight FWInherit)}
               ]


data FStretch = NormalStretch | Wider | Narrower | UltraCondensed | ExtraCondensed | Condensed
              | SemiCondensed | SemiExpanded | Expanded | ExtraExpanded | UltraExpanded | SInherit

styleFontStretch =
  do AT.skipSpace
     AT.string "font-stretch:"
     AT.choice [ do { string "normal";          return (FontStretch NormalStretch)}
               , do { string "wider";           return (FontStretch Wider)}
               , do { string "narrower";        return (FontStretch Narrower)}
               , do { string "ultra-condensed"; return (FontStretch UltraCondensed)}
               , do { string "extra-condensed"; return (FontStretch ExtraCondensed)}
               , do { string "condensed";       return (FontStretch Condensed)}
               , do { string "semi-condensed";  return (FontStretch SemiCondensed)}
               , do { string "semi-expanded";   return (FontStretch SemiExpanded)}
               , do { string "expanded";        return (FontStretch Expanded)}
               , do { string "extra-expanded";  return (FontStretch ExtraExpanded)}
               , do { string "ultra-expanded";  return (FontStretch UltraExpanded)}
               , do { string "inherit";         return (FontStretch SInherit)}
               ]

styleFontSize =
  do AT.skipSpace
     AT.string "font-size:"
     len <- styleLength
     return (FontSize len)

------------------------------------------------------------------------------------------------------------

-- | See <http://www.w3.org/TR/SVG/types.html#Length>
styleLength =
  do AT.skipSpace
     d <- myDouble
     AT.skipSpace
     AT.choice [ styleLengthWithUnit (fromRational $ toRational d),
                 lengthPercent (fromRational $ toRational d), return (Len (fromRational $ toRational d)) ]

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
  do (absrel,frag) <- parseIRI
     let t = H.lookup frag hmap
     if isJust t then return (ClipPath $ evalPath hmap Nothing (fromJust t))
                 else return EmptyStyle

-- | Evaluate the tree to a path. Is only needed for clipPaths
evalPath :: RealFloat n => H.HashMap Text (Tag b n) -> Maybe (ViewBox n) -> (Tag b n) -> Path V2 n
evalPath hmap (Just viewBox) (Leaf id1 path diagram)               = path viewBox
evalPath hmap Nothing        (Leaf id1 path diagram)               = path (0,0,1,1) -- shouldn't happen, there should always be a viewbox
evalPath hmap _       (SubTree _ id1 _ (Just viewBox) ar f children) = mconcat (map (evalPath hmap (Just viewBox)) children)
evalPath hmap (Just viewBox) (SubTree _ id1 _ Nothing ar f children) = mconcat (map (evalPath hmap (Just viewBox)) children)
-- evalPath hmap (Reference selfId id1 wh f) = evalPath hmap (lookUp hmap (fragment id1)) -- TODO implement (not that common)
evalPath hmap _ _ = mempty

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
  do l <- myDouble
     return $ StrokeMiterLimit $ (fromRational . toRational) l

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
     styleStrokeOpacityVal

styleStrokeOpacityVal =
  do l <- myDouble
     return $ StrokeOpacity $ (fromRational . toRational) l

styleStopColor =
  do AT.skipSpace
     AT.string "stop-color:"
     AT.skipSpace
     styleFillColourVal

styleStopOpacity =
  do AT.skipSpace
     AT.string "stop-opacity:"
     AT.skipSpace
     styleFillOpacityVal

-- TODO: Visibility, marker
-----------------------------------------------------------------------
-- Colors, see <http://www.w3.org/TR/SVG/color.html> and 
--             <http://www.w3.org/TR/SVG/painting.html#SpecifyingPaint>
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

colorRGBWord =
  do AT.string "rgb("
     AT.skipSpace
     r <- decimal
     AT.skipSpace
     AT.char ','
     AT.skipSpace
     g <- decimal
     AT.skipSpace
     AT.char ','
     AT.skipSpace
     b <- decimal
     AT.skipSpace
     AT.char ')'
     return $ opaque (sRGB ((fromIntegral r)/255) ((fromIntegral g)/255) ((fromIntegral b)/255))

colorRGBPercent =
  do AT.string "rgb("
     AT.skipSpace
     r <- decimal
     AT.char '%'
     AT.skipSpace
     AT.char ','
     AT.skipSpace
     g <- decimal
     AT.char '%'
     AT.skipSpace
     AT.char ','
     AT.skipSpace
     b <- decimal
     AT.char '%'
     AT.skipSpace
     AT.char ')'
     return $ opaque (sRGB ((fromIntegral r)/100) ((fromIntegral g)/100) ((fromIntegral b)/100))

colorHSLPercent =
  do AT.string "hsl("
     AT.skipSpace
     h <- decimal
     AT.char '%'
     AT.skipSpace
     AT.char ','
     AT.skipSpace
     s <- decimal
     AT.char '%'
     AT.skipSpace
     AT.char ','
     AT.skipSpace
     l <- decimal
     AT.char '%'
     AT.skipSpace
     AT.char ')'
     let c = hsl (fromIntegral h) (fromIntegral s) (fromIntegral l)
     return $ opaque (sRGB (channelRed c) (channelGreen c) (channelBlue c))

colorNone =
  do AT.string "none"
     return transparent

-------------------------------------------------------------------------------------
-- | Example: spreadMethod="pad"
parseSpread :: Maybe Text -> SpreadMethod
parseSpread spr | isJust parsedSpread = fromJust parsedSpread
                | otherwise = GradPad -- most of the time its "pad"
  where parsedSpread = parseTempl gradSpread spr

gradSpread = AT.choice [gradPad, gradReflect, gradRepeat ]

gradPad = do AT.string "pad"
             return GradPad

gradReflect = do AT.string "reflect"
                 return GradReflect

gradRepeat = do AT.string "repeat"
                return GradRepeat

-------------------------------------------------------------------------------------
-- | Example: viewBox="0 0 100 30"
--   Viewboxes establish a new viewport. Percentages (e.g. x="50%") only make sense with a viewport.
parseViewBox :: RealFloat n => Maybe Text -> Maybe Text -> Maybe Text -> Maybe (ViewBox n)
parseViewBox vb w h | isJust parsedVB    = parsedVB -- This is how it should always be, 
                                                    -- but sometimes an <svg>-tag has no viewbox attribute
                    | pw == 0 || ph == 0 = Nothing -- TODO: What does a browser do here?
                    | otherwise          = Just (0,0,pw, ph) -- If there is no viewbox the image size is the viewbox 
                                                             -- TODO: What does a browser do here?
                                                             -- The only other option I see is finding the min and max values of
                                                             -- shapes in user coordinate system, ignoring percentages
                                                             -- But one pass to just find out the viewbox?
  where parsedVB = parseTempl viewBox vb

        -- Assuming percentages are not used in width/height of the top <svg>-tag
        -- and there are no sub-<svg>-tags that use percentage-width/height to refer to their calling viewbox
        -- Using width and height is a hack anyway
        pw | isJust w = parseDouble $ fromJust w
           | otherwise = 0
        ph | isJust h = parseDouble $ fromJust h
           | otherwise = 0

viewBox =
  do AT.skipSpace
     minx <- myDouble
     AT.skipSpace
     miny <- myDouble
     AT.skipSpace
     width  <- myDouble
     AT.skipSpace
     height <- myDouble
     AT.skipSpace
     return ((fromRational . toRational) minx,
             (fromRational . toRational) miny,
             (fromRational . toRational) width,
             (fromRational . toRational) height)

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

