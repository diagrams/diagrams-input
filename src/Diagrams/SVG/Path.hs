{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------
-- Parsing the SVG path command, see <http://www.w3.org/TR/SVG/paths.html#PathData>
-------------------------------------------------------------------------------------

module Diagrams.SVG.Path
    (
    -- * Converting Path Commands
      commandsToTrails
    , splittedCommands
    , outline
    , nextSegment
    , svgArc
    -- * Parsing (Generating Path Commands)
    , PathCommand(..)
    , parsePathCommand
    , commands
    )
where

import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AT
import Data.Char (isAlpha, isHexDigit, digitToInt)
import Data.Colour.Names (readColourName)
import Data.Colour.SRGB
import qualified Data.List.Split as S
import Data.List (foldl')
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing, maybeToList, catMaybes)
import qualified Data.Text as T
import Data.Text(Text(..), pack, unpack, empty)
import Data.Tuple.Select
import Diagrams.Attributes
import Diagrams.Path
import Diagrams.Segment
import Diagrams.TwoD.Types
import Diagrams.Prelude
import Debug.Trace

data AbsRel = Abs | Rel deriving Show
data PathCommand n =
  M AbsRel (n,n) | -- ^AbsRel (x,y): Establish a new current point (with absolute coords)
  Z | -- ^Close current subpath by drawing a straight line from current point to current subpath's initial point
  L AbsRel (n,n) | -- ^AbsRel (X,Y): A line from the current point to Tup which becomes the new current point
  H AbsRel n | -- ^AbsRel x: A horizontal line from the current point (cpx, cpy) to (x, cpy)
  V AbsRel n | -- ^AbsRel y: A vertical line from the current point (cpx, cpy) to (cpx, y)
  C AbsRel (n,n,n,n,n,n) | -- ^AbsRel (X1,Y1,X2,Y2,X,Y): Draws a cubic Bézier curve from the current point to (x,y) using (x1,y1) as the
  -- ^control point at the beginning of the curve and (x2,y2) as the control point at the end of the curve.
  S AbsRel (n,n,n,n) | -- ^AbsRel (X2,Y2,X,Y): Draws a cubic Bézier curve from the current point to (x,y). The first control point is
-- assumed to be the reflection of the second control point on the previous command relative to the current point.
-- (If there is no previous command or if the previous command was not an C, c, S or s, assume the first control
-- point is coincident with the current point.) (x2,y2) is the second control point (i.e., the control point at
-- the end of the curve).
  Q AbsRel (n,n,n,n) | -- ^AbsRel (X1,Y1,X,Y): A quadr. Bézier curve from the curr. point to (x,y) using (x1,y1) as the control point.
-- Nearly the same as cubic, but with one point less
  T AbsRel (n,n) | -- ^AbsRel (X,Y): T_Abs = Shorthand/smooth quadratic Bezier curveto
  A AbsRel (n,n,n,n,n,n,n) -- ^AbsRel (rx,ry,xAxisRot,fl0,fl1,x,y): Elliptic arc
   deriving Show


-- | The parser to parse the lines and curves that make an outline
parsePathCommand = do { AT.skipSpace;
                        AT.choice [parse_m, parse_M, parse_l, parse_L, parse_h, parse_H,
                                   parse_v, parse_V, parse_c, parse_C, parse_S, parse_s,
                                   parse_q, parse_Q, parse_t, parse_T, parse_a, parse_A, parse_z]
                      }

-- Although it makes no sense, some programs produce several M in sucession
parse_m = do { AT.string "m"; t <- many' tuple2; return (Just $ map (M Rel) t) } -- that's why we need many'
parse_M = do { AT.string "M"; t <- many' tuple2; return (Just $ map (M Abs) t) }
parse_z = do { AT.choice [AT.string "z", AT.string "Z"]; return (Just [Z]) }
parse_l = do { AT.string "l"; t <- many' tuple2; return (Just $ map (L Rel) t) }
parse_L = do { AT.string "L"; t <- many' tuple2; return (Just $ map (L Abs) t) }
parse_h = do { AT.string "h"; t <- many' spaceDouble; return (Just $ map (H Rel) t) }
parse_H = do { AT.string "H"; t <- many' spaceDouble; return (Just $ map (H Abs) t) }
parse_v = do { AT.string "v"; t <- many' spaceDouble; return (Just $ map (V Rel) t) }
parse_V = do { AT.string "V"; t <- many' spaceDouble; return (Just $ map (V Abs) t) }
parse_c = do { AT.string "c"; t <- many' tuple6; return (Just $ map (C Rel) t) }
parse_C = do { AT.string "C"; t <- many' tuple6; return (Just $ map (C Abs) t) }
parse_s = do { AT.string "s"; t <- many' tuple4; return (Just $ map (S Rel) t) }
parse_S = do { AT.string "S"; t <- many' tuple4; return (Just $ map (S Abs) t) }
parse_q = do { AT.string "q"; t <- many' tuple4; return (Just $ map (Q Rel) t) }
parse_Q = do { AT.string "Q"; t <- many' tuple4; return (Just $ map (Q Abs) t) }
parse_t = do { AT.string "t"; t <- many' tuple2; return (Just $ map (T Rel) t) }
parse_T = do { AT.string "T"; t <- many' tuple2; return (Just $ map (T Abs) t) }
parse_a = do { AT.string "a"; t <- many' tuple7; return (Just $ map (A Rel) t) }
parse_A = do { AT.string "A"; t <- many' tuple7; return (Just $ map (A Abs) t) }

-- | In SVG values can be separated with a "," but don't have to be
withOptional parser a = do { AT.skipSpace;
                             AT.choice [ do { AT.char a; b <- parser; return b},
                                         do {            b <- parser; return b} ] }

doubleWithOptional a = do { d <- double `withOptional` a ; return (fromRational $ toRational d) }

spaceDouble = do { AT.skipSpace; d <- double; return (fromRational $ toRational d) }

tuple2 = do { a <- spaceDouble; b <- doubleWithOptional ','; return (a, b) }

tuple4 = do { a <- spaceDouble;
              b <- doubleWithOptional ',';
              c <- doubleWithOptional ',';
              d <- doubleWithOptional ',';
              return (a, b, c, d) }

tuple6 = do { a <- spaceDouble;
              b <- doubleWithOptional ',';
              c <- doubleWithOptional ',';
              d <- doubleWithOptional ',';
              e <- doubleWithOptional ',';
              f <- doubleWithOptional ','; return (a, b, c, d, e, f) }

tuple7 = do { a <- spaceDouble;
              b <- doubleWithOptional ',';
              c <- spaceDouble;
              AT.skipSpace; d <- decimal;
              e <- decimal `withOptional` ',';
              f <- spaceDouble;
              g <- doubleWithOptional ',';
              return (a, b, c, fromIntegral d, fromIntegral e, f, g) }


-- | Convert a path string into path commands
commands :: (RealFloat n) => Maybe Text -> [PathCommand n]
commands =  concat .
            catMaybes .
           (either (const []) id) .
           (AT.parseOnly (many' parsePathCommand)) .
           (fromMaybe T.empty)


-- | Convert path commands into trails
commandsToTrails :: (RealFloat n) => [PathCommand n] -> [Path V2 n]
commandsToTrails pathCommands = map fst $ foldl' outline [] (splittedCommands pathCommands)


-- | split list when there is a Z(closePath) and also when there is a (M)oveto command (keep the M)
--   and merge repeated lists of single Ms into one M command
splittedCommands pathCommands = concat $ map (S.split (S.keepDelimsR (S.whenElt isZ))) $ -- a path ends with a Z
                                mergeMs $                                 -- now it is one M
                                S.split (S.keepDelimsL (S.whenElt isM))   -- a path starts with Ms
                                pathCommands
  where
    isM (M ar p) = True
    isM _        = False
    isZ Z = True
    isZ _ = False
    -- single Ms are a problem, because we would move something empty that we don't remember.
    mergeMs :: RealFloat n => [[PathCommand n]] -> [[PathCommand n]]
    mergeMs ( [M Rel (x,y)] : ( ((M Rel (x0,y0)):cs):ds ) ) = mergeMs (((M Rel (x+x0,y+y0)):cs):ds)
    mergeMs ( [M Rel (x,y)] : ( ((M Abs (x0,y0)):cs):ds ) ) = mergeMs (((M Abs (x0,    y0)):cs):ds)
    mergeMs ( [M Abs (x,y)] : ( ((M Rel (x0,y0)):cs):ds ) ) = mergeMs (((M Abs (x+x0,y+y0)):cs):ds)
    mergeMs ( [M Abs (x,y)] : ( ((M Abs (x0,y0)):cs):ds ) ) = mergeMs (((M Abs (x0,    y0)):cs):ds)
    mergeMs (c:cs) = c : (mergeMs cs)
    mergeMs [] = []

data ClosedTrail a = O a | Closed a
isClosed (Closed _) = True
isClosed _          = False

getTrail (Closed a) = a
getTrail (O a)      = a

-- | Take the endpoint of the latest path, append another path that has been generated from the path commands
-- and return this whole path
outline :: RealFloat n => [(Path V2 n, (n, n))] -> [PathCommand n] -> [(Path V2 n, (n, n))]
outline paths cs = paths ++ [(newPath,newPoint)]
 where
  newPath = translate (r2 (trx,try)) $
            pathFromTrail $
            if isClosed (sel3 concatPaths)
            then wrapLoop $ closeLine (mconcat (getTrail (sel3 concatPaths)))
            else wrapLoop $ closeLine (mconcat (getTrail (sel3 concatPaths))) -- unfortunately this has to be closed also, 
                                                                              -- because some svgs fill paths that are open

  newPoint | isClosed (sel3 concatPaths) = (trx, try) -- the endpoint is the old startpoint
           | otherwise                   = sel2 concatPaths

  concatPaths = foldl' nextSegment ((x,y), (x,y), O []) cs

--  traceP (contr,point,path) = Debug.Trace.trace (show point) (contr,point,path)

  (trx,try) | null cs   = (0,0)
            | otherwise = sel2 $ nextSegment ((x,y), (x,y), O []) (head cs) -- cs usually always starts with a M-command,
                                                                            -- because we splitted the commands like that
  (x,y) | null paths = (0,0)
        | otherwise  = snd (last paths)


-- | The last control point and end point of the last path are needed to calculate the next line to append
--             endpoint -> (controlPoint, startPoint, line) ->
nextSegment :: RealFloat n => ((n,n), (n,n), ClosedTrail [Trail' Line V2 n]) -> PathCommand n -> ( (n,n), (n,n), ClosedTrail [Trail' Line V2 n])
nextSegment (ctrlPoint, startPoint, O trail) Z  = (ctrlPoint, startPoint, Closed trail)
nextSegment (_, _,       _      ) (M Abs point) = (point, point, O [])
nextSegment (_, (x0,y0), _      ) (M Rel (x,y)) = ((x+x0, y+y0), (x+x0, y+y0), O [])
nextSegment (_, (x0,y0), O trail) (L Abs (x,y)) = ((x,    y   ), (x,    y   ), O $ trail ++ [straight' (x-x0, y-y0)])
nextSegment (_, (x0,y0), O trail) (L Rel (x,y)) = ((x+x0, y+y0), (x+x0, y+y0), O $ trail ++ [straight' (x,    y   )])
nextSegment (_, (x0,y0), O trail) (H Abs x)     = ((x,      y0), (x,      y0), O $ trail ++ [straight' (x-x0,    0)])
nextSegment (_, (x0,y0), O trail) (H Rel x)     = ((x+x0,   y0), (x+x0,   y0), O $ trail ++ [straight' (x,       0)])
nextSegment (_, (x0,y0), O trail) (V Abs y)     = ((  x0, y   ), (  x0, y   ), O $ trail ++ [straight' (0 ,   y-y0)])
nextSegment (_, (x0,y0), O trail) (V Rel y)     = ((  x0, y+y0), (  x0, y+y0), O $ trail ++ [straight' (0,    y   )])

nextSegment (_, (x0,y0), O trail) (C Abs (x1,y1,x2,y2,x,y)) = ((x2,y2), (x,y), O $ trail ++ [bez3 (x1-x0, y1-y0) (x2-x0, y2-y0) (x-x0,y-y0)])
nextSegment (_, (x0,y0), O trail) (C Rel (x1,y1,x2,y2,x,y)) = ((x2+x0, y2+y0), (x+x0, y+y0), O $ trail ++ [bez3 (x1, y1) (x2, y2) (x,y)])

nextSegment ((cx,cy),(x0,y0), O trail) (S Abs (x2,y2,x,y)) = ((x2, y2), (x, y), O $ trail ++ [bez3 (x0-cx, y0-cy) (x2-x0, y2-y0) (x-x0, y-y0)])
nextSegment ((cx,cy),(x0,y0), O trail) (S Rel (x2,y2,x,y)) = ((x2+x0, y2+y0), (x+x0, y+y0), O $ trail ++ [bez3 (x0-cx, y0-cy) (x2, y2) (x, y)])

nextSegment (_, (x0,y0), O trail) (Q Abs (x1,y1,x,y)) = ((x1, y1),       (x, y), O $ trail ++ [bez3 (x1-x0, y1-y0) (x-x0, y-y0) (x-x0, y-y0)])
nextSegment (_, (x0,y0), O trail) (Q Rel (x1,y1,x,y)) = ((x1+x0, y1+y0), (x+x0, y+y0), O $ trail ++ [bez3 (x1, y1) (x, y) (x, y)])

nextSegment ((cx,cy), (x0,y0), O trail) (T Abs (x,y)) = ((2*x0-cx, 2*y0-cy ), (x, y), O $ trail ++ [bez3 (x0-cx, y0-cy) (x-x0, y-y0) (x-x0, y-y0)])
nextSegment ((cx,cy), (x0,y0), O trail) (T Rel (x,y)) = ((2*x0-cx, 2*y0-cy),  (x, y), O $ trail ++ [bez3 (x0-cx, y0-cy) (x, y) (x, y)])

nextSegment (_, (x0,y0), O trail) (A Abs (rx,ry,xAxisRot,fl0,fl1,x,y) ) = (nul, nul, O $ trail ++ [svgArc (rx,ry) xAxisRot fl0 fl1 (x, y)])
nextSegment (_, (x0,y0), O trail) (A Rel (rx,ry,xAxisRot,fl0,fl1,x,y) ) = (nul, nul, O $ trail ++ [svgArc (rx,ry) xAxisRot fl0 fl1 (x, y)])

nul = (0,0)

straight' = lineFromSegments . (:[]) . straight . r2

bez3 point1 point2 point3 = lineFromSegments [bezier3 (r2 point1) (r2 point2) (r2 point3)]

-- | The arc command: see http://www.w3.org/TR/SVG11/implnote.html#ArcImplementationNotes
-- To Do: scale if rx,ry,xAxisRot are such that there is no solution
svgArc :: RealFloat n => (n, n) -> n -> n -> n -> (n,n) -> Trail' Line V2 n
svgArc (rxx, ryy) xAxisRot largeArcFlag sweepFlag (x2, y2)
     | x2 == 0 && y2 == 0 = emptyLine -- spec F6.2
     | rx == 0 || ry == 0 = straight' (x2,y2) -- spec F6.2
--     | otherwise = arc' (-rx) (theta1 @@ rad) ((theta1 + dtheta21) @@ rad) # scaleY (ry/rx) # rotateBy xAxisRot
  where rx | rxx < 0   = -rxx  -- spec F6.2
           | otherwise =  rxx
        ry | ryy < 0   = -ryy  -- spec F6.2
           | otherwise =  ryy
        fa | largeArcFlag == 0 = 0
           | otherwise         = 1 -- spec F6.2
        fs | sweepFlag == 0 = 0
           | otherwise      = 1 -- spec F6.2
        (x1,y1) = (0,0)
        x1x2 = (x1 - x2)/2
        y1y2 = (y1 - y2)/2
        x1' =  (cos xAxisRot) * x1x2 + (sin xAxisRot) * y1y2
        y1' = -(sin xAxisRot) * x1x2 + (cos xAxisRot) * y1y2
        s = (rx*rx*ry*ry - rx*rx*y1'*y1' - ry*ry*x1'*x1') / ( rx*rx*y1'*y1' + ry*ry*x1'*x1' )
        root = sqrt ( (rx*rx*ry*ry - rx*rx*y1'*y1' - ry*ry*x1'*x1') / ( rx*rx*y1'*y1' + ry*ry*x1'*x1' ) )
        cx' | fa /= fs  =   root * rx * y1' / ry
            | otherwise = - root * rx * y1' / ry
        cy' | fa /= fs  = - root * ry * x1' / rx
            | otherwise =   root * ry * x1' / rx
        cx = (cos xAxisRot) * cx' - (sin xAxisRot) * cy' + ((x1+x2)/2)
        cy = (sin xAxisRot) * cx' + (cos xAxisRot) * cy' + ((y1+y2)/2)
        angle u v = (signof u v) * (acos (  (scalar u v) / ( (len u) * (len v) )  ))
        signof (ux,uy) (vx,vy) | ux * vy - uy * vx > 0 =   1
                               | otherwise             = - 1
        theta1 = angle (1,0) ((x1'-cx')/rx, (y1'-cy')/ry)
        dtheta = angle ((x1'-cx')/rx, (y1'-cy')/ry)  ((-x1'-cx')/rx, (-y1'-cy')/ry)
        dtheta21 | fs == 0   = if dtheta > 0 then dtheta - (2*pi) else dtheta
                 | otherwise = if dtheta < 0 then dtheta + (2*pi) else dtheta
        scalar (ux,uy) (vx,vy) = ux * vx + uy * vy
        len (x,y) = sqrt (x*x + y*y)

