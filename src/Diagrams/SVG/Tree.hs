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
    )
where
import Data.Maybe (isJust, fromJust)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Data.Text(Text(..))
import Diagrams.Prelude
-- import Debug.Trace

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
     | Reference Id Id (Maybe n, Maybe n) ((HashMaps b n, ViewBox n) -> Diagram b -> Diagram b)-- ^
--  A reference (\<use\>-tag) consists of:
--
-- * An Id
--
-- * A reference to an Id
--
-- * Transformations applied to the reference
     | SubTree Bool Id (Maybe (ViewBox n)) (Maybe PreserveAR) (HashMaps b n -> Diagram b -> Diagram b) [Tag b n]-- ^
-- A subtree consists of:
--
-- * A Bool: Are we in a section that will be rendered directly (not in a \<defs\>-section)
--
-- * An Id of subdiagram
--
-- * A transformation or application of a style to a subdiagram
--
-- * A list of subtrees
     | StyleTag [(Text, [(Text, Text)])] -- ^ A tag that contains CSS styles with selectors and attributes
     | Grad Id (Maybe (ViewBox n)) ((CSSMap,ViewBox n) -> Texture n) -- ^ A radial gradient
     | Stop (HashMaps b n -> [GradientStop n]) -- ^
-- We need to make this part of this data structure because Gradient tags can also contain description tags

type Id      = Maybe Text
type Attrs = [(Text, Text)]

type Nodelist b n = [(Text, Tag b n)]
type CSSlist  = [(Text, Attrs)]
data Gr n = Gr (Maybe (ViewBox n)) ((CSSMap,ViewBox n) -> Texture n)
type Gradlist n = [(Text, Gr n)]

type HashMaps b n = (NodesMap b n, CSSMap, GradientsMap n)
type NodesMap b n = H.HashMap Text (Tag b n)
type CSSMap = H.HashMap Text Attrs
type GradientsMap n = H.HashMap Text (Texture n)

type ViewBox n = (n,n,n,n) -- (MinX,MinY,Width,Height)

data PreserveAR = PAR AlignSVG MeetOrSlice -- ^ see <http://www.w3.org/TR/SVG11/coords.html#PreserveAspectRatioAttribute>
data AlignSVG = AlignXY Place Place -- ^ alignment in x and y direction
type Place = Double -- ^ A value between 0 and 1, where 0 is the minimal value and 1 the maximal value
data MeetOrSlice = Meet | Slice

instance Show (Tag b n) where
  show (Leaf id1 _ _)  = "Leaf "      ++ (show id1) ++ "\n"
  show (Reference selfid id1 wh f) = "Reference " ++ (show id1) ++ "\n"
  show (SubTree b id1 viewbox ar f tree) = "Sub " ++ (show id1) ++ concat (map show tree) ++ "\n"
  show (StyleTag _)   = "Style "    ++ "\n"
  show (Grad id1 vb tex)   = "Grad "   ++ (show id1) ++ "\n"
  show (Stop _)   = "Stop "         ++ "\n"

----------------------------------------------------------------------------------
-- | Generate elements that can be referenced by their ID.
--   The tree nodes are splitted into 3 groups of lists of (ID,value)-pairs):
--
-- * Nodes that contain elements that can be transformed to a diagram
--
-- * CSS classes with corresponding (attribute,value)-pairs, from the <defs>-tag
--
-- * Gradients
nodes :: Show n => Maybe (ViewBox n) -> (Nodelist b n, CSSlist, Gradlist n) -> Tag b n -> (Nodelist b n, CSSlist, Gradlist n)
nodes viewbox (ns,css,grads) (Leaf id1 path diagram)
  | isJust id1 = (ns ++ [(fromJust id1, Leaf id1 path diagram)],css,grads)
  | otherwise  = (ns,css,grads)

-- A Reference element for the <use>-tag
nodes viewbox (ns,css,grads) (Reference selfId id1 wh f) = (ns,css,grads)

nodes viewbox (ns,css,grads)                      (SubTree b id1 Nothing ar f children)
  | isJust id1 = myconcat [ (ns ++ [(fromJust id1, SubTree b id1 viewbox ar f children)],css,grads) ,
                            (myconcat (map (nodes viewbox (ns,css,grads)) children))                ]
  | otherwise  = myconcat (map (nodes viewbox (ns,css,grads)) children)

nodes viewbox (ns,css,grads)                      (SubTree b id1 vb ar f children)
  | isJust id1 = myconcat [ (ns ++ [(fromJust id1, SubTree b id1 vb ar f children)],css,grads) ,
                            (myconcat (map (nodes vb (ns,css,grads)) children))                ]
  | otherwise  = myconcat (map (nodes vb (ns,css,grads)) children)

nodes viewbox (ns,css,grads) (Grad id1 vb texture)
  | isJust id1 = (ns,css, grads ++ [(fromJust id1, Gr viewbox texture)] )
  | otherwise  = (ns,css,grads)


-- There is a global style tag in the defs section of some svg files
nodes viewbox (ns,css,grads) (StyleTag styles) = (ns,css ++ styles,grads)
-- stops are not extracted here but from the gradient parent node
nodes viewbox lists (Stop _) = lists

myconcat :: [(Nodelist b n, CSSlist, Gradlist n)] -> (Nodelist b n, CSSlist, Gradlist n)
myconcat list = (concat $ map sel1 list, concat $ map sel2 list, concat $ map sel3 list)
  where sel1 (a,b,c) = a
        sel2 (a,b,c) = b
        sel3 (a,b,c) = c

