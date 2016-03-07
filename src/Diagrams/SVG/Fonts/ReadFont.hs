module Diagrams.SVG.Fonts.ReadFont
       (
         FontData(..)
       , FontFace(..)
       , Kern(..)
       , KernDir(..)
       , FontContent(..)

       , parseBBox
--       , bbox_dy
--       , bbox_lx, bbox_ly

--       , underlinePosition
--       , underlineThickness

       , kernMap
       , horizontalAdvance
--       , kernAdvance

       , OutlineMap
       , PreparedFont
       ) where

import           Data.Char         (isSpace)
import           Data.List        (intersect, sortBy)
import           Data.List.Split  (splitOn, splitWhen)
import qualified Data.HashMap.Strict as H
import           Data.Maybe       (catMaybes, fromJust, fromMaybe, 
                                  isJust, isNothing, maybeToList)
import qualified Data.Text           as T
import           Data.Text        (Text(..), pack, unpack, empty, words)
import           Data.Text.Read   (double)
import           Data.Vector      (Vector)
import qualified Data.Vector      as V
import           Diagrams.Path
import           Diagrams.Prelude hiding (font)

import           Diagrams.SVG.Fonts.CharReference (charsFromFullName)
import           Diagrams.SVG.Path
import           Diagrams.SVG.Tree

kernMap :: [Kern n] -> KernMaps n
kernMap kernlist = KernMaps [] H.empty H.empty H.empty H.empty V.empty -- (transformChars (map kernU1)
  where
    transformChars chars = H.fromList $ map ch $ multiSet $ map (\(x,y) -> (x,[y])) $ sort fst $ 
                           concat $ addIndex chars -- e.g. [["aa","b"],["c","d"]] to [("aa",0),("b",0),("c",1), ("d",1)]
    ch (x,y) | null x = ("",y)
             | otherwise = (x,y)

    addIndex qs = zipWith (\x y -> (map (\z -> (z,x)) y)) [0..] qs
    sort f xs = sortBy (\x y -> compare (f x) (f y) ) xs

    multiSet [] = []
    multiSet (a:[]) = [a] -- example: [("n1",[0]),("n1",[1]),("n2",[1])] to [("n1",[0,1]),("n2",[1])]
    multiSet (a:b:bs) | fst a == fst b = multiSet ( (fst a, (snd a) ++ (snd b)) : bs)
                      | otherwise = a : (multiSet (b:bs))

    fname f = last $ init $ concat (map (splitOn "/") (splitOn "." f))


parseBBox :: (Read n, RealFloat n) => Maybe Text -> [n]
parseBBox bbox = maybe [] ((map convertToN) . T.words) bbox
  where convertToN = fromRational . toRational . (either (const 0) fst) . double

-- glyphs = map glyphsWithDefaults glyphElements

    -- monospaced fonts sometimes don't have a "horiz-adv-x="-value , replace with "horiz-adv-x=" in <font>
-- glyphsWithDefaults g = (charsFromFullName $ fromMaybe gname (findAttr (unqual "unicode") g), -- there is always a name or unicode
--                         (
--                           gname,
--                           fromMaybe fontHadv (fmap read (findAttr (unqual "horiz-adv-x") g)),
--                           fromMaybe "" (findAttr (unqual "d") g)
--                         )
--                       )
--      where gname = fromMaybe "" (findAttr (unqual "glyph-name") g)

-- | Horizontal advance of a character consisting of its width and spacing, extracted out of the font data
horizontalAdvance :: RealFloat n => Text -> FontData b n -> n
horizontalAdvance ch fontD
    | isJust char = (\(a,b,c) -> b) (fromJust char)
    | otherwise   = fontDataHorizontalAdvance fontD
  where char = (H.lookup ch (fontDataGlyphs fontD))

-- | See <http://www.w3.org/TR/SVG/fonts.html#KernElements>
--
-- Some explanation how kerning is computed:
--
-- In Linlibertine.svg, there are two groups of chars: e.g.
-- \<hkern g1=\"f,longs,uni1E1F,f_f\" g2=\"parenright,bracketright,braceright\" k=\"-37\" />
-- This line means: If there is an f followed by parentright, reduce the horizontal advance by -37 (add 37).
-- Therefore to quickly check if two characters need kerning assign an index to the second group (g2 or u2)
-- and assign to every unicode in the first group (g1 or u1) this index, then sort these tuples after their
-- name (for binary search). Because the same unicode char can appear in several g1s, reduce this 'multiset',
-- ie all the (\"name1\",0) (\"name1\",1) to (\"name1\",[0,1]).
-- Now the g2s are converted in the same way as the g1s.
-- Whenever two consecutive chars are being printed try to find an
-- intersection of the list assigned to the first char and second char

-- | Change the horizontal advance of two consective chars (kerning)
{-
kernAdvance :: RealFloat n => String -> String -> KernMaps n -> Bool -> n
kernAdvance ch0 ch1 kern u |     u && not (null s0) = (kernK kern) V.! (head s0)
                           | not u && not (null s1) = (kernK kern) V.! (head s1)
                           | otherwise = 0
  where s0 = intersect (s kernU1S ch0) (s kernU2S ch1)
        s1 = intersect (s kernG1S ch0) (s kernG2S ch1)
        s sel ch = concat (maybeToList (Map.lookup ch (sel kern)))
-}
-- > import Graphics.SVGFonts.ReadFont
-- > textWH0 = (rect 8 1) # alignBL <> ((textSVG_ $ TextOpts "SPACES" lin INSIDE_WH KERN False 8 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd) # alignBL
-- > textWH1 = (rect 8 1) # alignBL <> ((textSVG_ $ TextOpts "are sometimes better." lin INSIDE_WH KERN False 8 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd) # alignBL
-- > textWH2 = (rect 8 1) # alignBL <> ((textSVG_ $ TextOpts "But too many chars are not good." lin INSIDE_WH KERN False 8 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd) # alignBL
-- > textWH = textWH0 # alignBL === strutY 0.3 === textWH1 === strutY 0.3 === textWH2 # alignBL
-- > textW0 = (rect 3 1) # alignBL <> ( (textSVG_ $ TextOpts "HEADLINE" lin INSIDE_W KERN False 3 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd ) # alignBL
-- > textW1 = (rect 10 1) # alignBL <> ( (textSVG_ $ TextOpts "HEADLINE" lin INSIDE_W KERN False 10 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd ) # alignBL
-- > textW = textW0 # alignBL ||| strutX 1 ||| textW1 # alignBL
-- > textH0 = (rect 10 1) # alignBL <> ((textSVG_ $ TextOpts "Constant font size" lin INSIDE_H KERN False 10 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd) # alignBL
-- > textH1 = (rect 3 1) # alignBL <> ((textSVG_ $ TextOpts "Constant font size" lin INSIDE_H KERN False 3 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd) # alignBL
-- > textH = textH0 # alignBL === strutY 0.5 === textH1 # alignBL

-- > import Graphics.SVGFonts.ReadFont
-- > textHADV = (textSVG_ $ TextOpts "AVENGERS" lin INSIDE_H HADV False 10 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd

-- > import Graphics.SVGFonts.ReadFont
-- > textKern = (textSVG_ $ TextOpts "AVENGERS" lin INSIDE_H KERN False 10 1 )
-- >              # fc blue # lc blue # bg lightgrey # fillRule EvenOdd

{-
-- | Difference between highest and lowest y-value of bounding box
bbox_dy :: RealFloat n => FontData n -> n
bbox_dy fontData = (bbox!!3) - (bbox!!1)
  where bbox = fontDataBoundingBox fontData -- bbox = [lowest x, lowest y, highest x, highest y]

-- | Lowest x-value of bounding box
bbox_lx :: RealFloat n => FontData n -> n
bbox_lx fontData   = (fontDataBoundingBox fontData) !! 0

-- | Lowest y-value of bounding box
bbox_ly :: RealFloat n => FontData n -> n
bbox_ly fontData   = (fontDataBoundingBox fontData) !! 1

-- | Position of the underline bar
underlinePosition :: RealFloat n => FontData n -> n
underlinePosition fontData = fontDataUnderlinePos fontData

-- | Thickness of the underline bar
underlineThickness :: RealFloat n => FontData n -> n
underlineThickness fontData = fontDataUnderlineThickness fontData
-}
-- | A map of unicode characters to outline paths.
type OutlineMap n = H.HashMap Text (Path V2 n)

-- | A map of unicode characters to parsing errors.
type ErrorMap = H.HashMap Text Text

-- | A font including its outline map.
type PreparedFont b n = (FontData b n, OutlineMap n)

-- | Compute a font's outline map, collecting errors in a second map.
outlineMap :: (Read n, Show n, RealFloat n) =>
              FontData b n -> (OutlineMap n, ErrorMap)
outlineMap fontData =
    ( H.fromList [(ch, outl) | (ch, Right outl) <- allOutlines]
    , H.fromList [(ch, err)  | (ch, Left err)   <- allOutlines]
    )
  where
    allUnicodes = H.keys (fontDataGlyphs fontData)
    outlines ch = return $ mconcat $ commandsToPaths (commandsFromChar ch (fontDataGlyphs fontData))
    allOutlines = [(ch, outlines ch) | ch <- allUnicodes]

-- | Prepare font for rendering, by determining its outline map.
prepareFont :: (Read n, Show n, RealFloat n) =>
               FontData b n -> (PreparedFont b n, ErrorMap)
prepareFont fontData = ((fontData, outlines), errs)
  where
    (outlines, errs) = outlineMap fontData

commandsFromChar :: RealFloat n => Text -> SvgGlyphs n -> [PathCommand n]
commandsFromChar ch glyph = case H.lookup ch glyph of
--    Just e  -> commands (Just $ pack $ (\(a,b,c) -> c) e)
    Nothing -> []

