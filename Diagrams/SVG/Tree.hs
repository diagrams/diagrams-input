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
    )
where
import Data.Maybe (isJust, fromJust)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Data.Text(Text(..))
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Tuple.Select

-------------------------------------------------------------------------------------
-- | A tree structure is needed to handle refences to parts of the tree itself.
-- The \<defs\>-section contains shapes that can be refered to, but the SVG standard allows to refer to
-- every tag in the SVG-file.
-- 
data Tag = Leaf Id (Path R2) ((NodesMap, CSSMap, GradientsMap) -> Diagram B R2)-- ^
-- A leaf consists of
--
-- * An Id
--
-- * A path so that this leaf can be used to clip some other part of a tree
--
-- * A diagram (Another option would have been to apply a function to the upper path)
     | Reference Id Id (Maybe Width, Maybe Height) (HashMaps -> Diagram B R2 -> Diagram B R2)-- ^
--  A reference (\<use\>-tag) consists of:
--
-- * An Id
--
-- * A reference to an Id
--
-- * Transformations applied to the reference
     | SubTree Bool Id (Maybe ViewBox) (Maybe PreserveAR) (HashMaps -> Diagram B R2 -> Diagram B R2) [Tag]-- ^
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
     | Grad Id (CSSMap -> Texture) -- ^ A radial gradient
     | Stop (HashMaps -> [GradientStop]) -- ^
-- We need to make this part of this data structure because Gradient tags can also contain description tags

type Id      = Maybe Text
type Attrs = [(Text, Text)]

type Nodelist = [(Text, Tag)]
type CSSlist  = [(Text, Attrs)]
type Gradlist  = [(Text, (H.HashMap Text Attrs -> Texture))]

type HashMaps = (NodesMap, CSSMap, GradientsMap)
type NodesMap = H.HashMap Text Tag
type CSSMap = H.HashMap Text Attrs
type GradientsMap = H.HashMap Text Texture

type ViewBox = (MinX,MinY,Width,Height)
type MinX = Double
type MinY = Double
type Width = Double
type Height = Double

data PreserveAR = PAR AlignSVG MeetOrSlice -- ^ see <http://www.w3.org/TR/SVG11/coords.html#PreserveAspectRatioAttribute>
data AlignSVG = AlignXY Place Place -- ^ alignment in x and y direction
type Place = Double -- ^ A value between 0 and 1, where 0 is the minimal value and 1 the maximal value
data MeetOrSlice = Meet | Slice

instance Show Tag where
  show (Leaf id1 path diagram)  = "Leaf "      ++ (show id1) ++ (show path) ++ "\n"
  show (Reference selfid id1 wh f) = "Reference " ++ (show id1) ++ "\n"
  show (SubTree b id1 viewbox ar f tree) = "Sub " ++ (show id1) ++ concat (map show tree) ++ "\n"
  show (StyleTag _)   = "Style "    ++ "\n"
  show (Grad id1 tex)   = "Grad "   ++ (show id1) ++ "\n"
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
nodes :: (Nodelist, CSSlist, Gradlist) -> Tag -> (Nodelist, CSSlist, Gradlist)
nodes (ns,css,grads) (Leaf id1 path diagram)
  | isJust id1 = (ns ++ [(fromJust id1, Leaf id1 path diagram)],css,grads)
  | otherwise  = (ns,css,grads)
nodes (ns,css,grads) (Grad id1 texture)
  | isJust id1 = (ns,css,grads ++ [(fromJust id1, texture)] )
  | otherwise  = (ns,css,grads)

-- A Reference element for the <use>-tag
nodes (ns,css,grads) (Reference selfId id1 wh f) = (ns,css,grads)
nodes (ns,css,grads) (SubTree b id1 viewbox ar f children)
  | isJust id1 = myconcat [ (ns ++ [(fromJust id1, SubTree b id1 viewbox ar f children)],css,grads) ,
                            (myconcat (map (nodes (ns,css,grads)) children))
                          ]
  | otherwise  = myconcat (map (nodes (ns,css,grads)) children)

-- There is a global style tag in the defs section of some svg files
nodes (ns,css,grads) (StyleTag styles) = (ns,css ++ styles,grads)
-- stops are not extracted here but from the gradient parent node
nodes lists (Stop _) = lists

myconcat :: [(Nodelist, CSSlist, Gradlist)] -> (Nodelist, CSSlist, Gradlist)
myconcat list = (concat $ map sel1 list, concat $ map sel2 list, concat $ map sel3 list)

