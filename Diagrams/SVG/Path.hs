{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------
-- Parsing the SVG path command, see <http://www.w3.org/TR/SVG/paths.html#PathData>
-------------------------------------------------------------------------------------

module Diagrams.SVG.Path where

import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AT
import Data.Char (isAlpha, isHexDigit, digitToInt)
import Data.Colour.Names (readColourName)
import Data.Colour.SRGB
import qualified Data.List.Split as S
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

type X = Double
type Y = Double
type X1 = X
type Y1 = Y
type X2 = X
type Y2 = Y
data AbsRel = Abs | Rel deriving Show
data PathCommand =
  M AbsRel (X,Y) | -- ^Establish a new current point (with absolute coords)
  Z | -- ^Close current subpath by drawing a straight line from current point to current subpath's initial point
  L AbsRel (X,Y) | -- ^A line from the current point to Tup which becomes the new current point
  H AbsRel X | -- ^A horizontal line from the current point (cpx, cpy) to (x, cpy)
  V AbsRel Y | -- ^A vertical line from the current point (cpx, cpy) to (cpx, y)
  C AbsRel (X1,Y1,X2,Y2,X,Y) | -- ^Draws a cubic Bézier curve from the current point to (x,y) using (x1,y1) as the
  -- ^control point at the beginning of the curve and (x2,y2) as the control point at the end of the curve.
  S AbsRel (X2,Y2,X,Y) | -- ^Draws a cubic Bézier curve from the current point to (x,y). The first control point is
-- assumed to be the reflection of the second control point on the previous command relative to the current point.
-- (If there is no previous command or if the previous command was not an C, c, S or s, assume the first control
-- point is coincident with the current point.) (x2,y2) is the second control point (i.e., the control point at
-- the end of the curve).
  Q AbsRel (X1,Y1,X,Y) | -- ^A quadr. Bézier curve from the curr. point to (x,y) using (x1,y1) as the control point.
-- Nearly the same as cubic, but with one point less
  T AbsRel (X,Y) | -- ^T_Abs = Shorthand/smooth quadratic Bezier curveto
  A AbsRel (Double,Double,Double,Int,Int,Double,Double) -- ^A = Elliptic arc (not used)
   deriving Show


-- | The parser to parse the lines and curves that make an outline
parsePathCommand = do { AT.skipSpace;
                        AT.choice [parse_m, parse_M, parse_l, parse_L, parse_h, parse_H,
                                   parse_v, parse_V, parse_c, parse_C, parse_S, parse_s,
                                   parse_q, parse_Q, parse_t, parse_T, parse_a, parse_A, parse_z]
                      }

-- although it makes no sense, some programs produce several M in sucession
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
doubleWithOptional a = do { AT.skipSpace;
                            AT.choice [ do { AT.char a; b <- double; return b},
                                        do {            b <- double; return b} ] }

spaceDouble = do { AT.skipSpace; a <- double; return a }

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
              AT.char ',';  e <- decimal;
              f <- spaceDouble;
              g <- doubleWithOptional ',';
              return (a, b, c, d, e, f, g) }

-- | I don't know of a single program that uses these arcs (not finished)
svgArc rx ry xAxisRot largeArcFlag sweepFlag x y = moveOriginBy c (arc' (sqrt rsquared) angleStart angleEnd)
  where -- the middle between the startpoint (a=0) and endpoint (b) is b/2
        c = (r2 (x/2, y/2)) ^+^ ( orth ^* (sqrt (rsquared - bsquared/4)))
        angleStart | largeArcFlag = (1/4 @@ turn) -- not finished
                   | otherwise = (1/4 @@ turn) -- not finished
        angleEnd | largeArcFlag = (1/4 @@ turn) -- not finished
                 | otherwise = (1/4 @@ turn) -- not finished
        rsquared = rx*rx + ry*ry -- radius of the circle squared
        bsquared = x*x + y*y -- b is the endpoint of the arc
        orth | sweepFlag = r2 (-y/l, x/l) -- orthogonal vector of length 1
             | otherwise = negateV (r2 (-y/l, x/l)) 
        l = sqrt bsquared


-- | Convert a path string into path commands
commands :: Maybe Text -> [PathCommand]
commands =  concat .
            catMaybes .
           (either (const []) id) .
           (AT.parseOnly (many' parsePathCommand)) .
           (fromMaybe empty)


-- | Convert path commands into trails
commandsToTrails :: [PathCommand] -> [Path R2]
commandsToTrails pathCommands = map fst $ foldl outline [] (splittedCommands pathCommands)


-- | split list when there is a (M)oveto command (and keep the M)
splittedCommands pathCommands = mergeSingleMs $ S.split (S.keepDelimsL (S.whenElt isM)) pathCommands
  where
    isM (M ar p) = True
    isM _        = False
    -- single Ms are a problem, because we would move something empty that we don't remember.
    mergeSingleMs :: [[PathCommand]] -> [[PathCommand]]
    mergeSingleMs ( [M Rel (x,y)] : ( ((M Rel (x0,y0)):cs):ds ) ) = mergeSingleMs (((M Rel (x+x0,y+y0)):cs):ds)
    mergeSingleMs ( [M Rel (x,y)] : ( ((M Abs (x0,y0)):cs):ds ) ) = mergeSingleMs (((M Abs (x0,    y0)):cs):ds)
    mergeSingleMs ( [M Abs (x,y)] : ( ((M Rel (x0,y0)):cs):ds ) ) = mergeSingleMs (((M Abs (x+x0,y+y0)):cs):ds)
    mergeSingleMs ( [M Abs (x,y)] : ( ((M Abs (x0,y0)):cs):ds ) ) = mergeSingleMs (((M Abs (x0,    y0)):cs):ds)
    mergeSingleMs (c:cs) = c : (mergeSingleMs cs)
    mergeSingleMs [] = []

-- | Take the endpoint of the latest path, append another path that has been generated from the path commands and return this whole path
outline :: [(Path R2, (Double, Double))] -> [PathCommand] -> [(Path R2, (Double, Double))]
outline paths cs = paths ++ [(newPath,newPoint)]
 where
  newPath = translate (r2 trans) $
            pathFromTrail $
            wrapTrail $
            closeLine $
            lineFromSegments (sel3 concatPaths)
  newPoint = sel2 concatPaths

  concatPaths = foldl nextSegment ((x, y), (x, y), [], M Rel (0,0)) cs

  traceP (contr,point,path,c) = Debug.Trace.trace (show point) (contr,point,path,c)

  trans | null cs   = (0,0)
        | otherwise = sel2 $ nextSegment ((x, y), (x, y), [], head cs) Z -- cs usually always starts with a M-command,
                                                                         -- because we splitted the commands like that
  (x,y) | null paths = (0,0)
        | otherwise  = snd (last paths)


-- | The last control point and end point of the last path are needed to calculate the next lines and curves
nextSegment (controlPoint, startPoint, path, Z) command = (controlPoint, startPoint, path, command)
nextSegment (_, _,       _,    M Abs point) c = (point, point, [], c)
nextSegment (_, (x0,y0), _,    M Rel (x,y)) c = ((x+x0, y+y0), (x+x0, y+y0), [], c)
nextSegment (_, (x0,y0), path, L Abs (x,y)) c = ((x,    y   ), (x,    y   ), path ++ [straight' (x-x0, y-y0)], c)
nextSegment (_, (x0,y0), path, L Rel (x,y)) c = ((x+x0, y+y0), (x+x0, y+y0), path ++ [straight' (x,    y   )], c)
nextSegment (_, (x0,y0), path, H Abs x) c     = ((x,      y0), (x,      y0), path ++ [straight' (x-x0,    0)], c)
nextSegment (_, (x0,y0), path, H Rel x) c     = ((x+x0,   y0), (x+x0,   y0), path ++ [straight' (x,       0)], c)
nextSegment (_, (x0,y0), path, V Abs y) c     = ((  x0, y   ), (  x0, y   ), path ++ [straight' (0 ,   y-y0)], c)
nextSegment (_, (x0,y0), path, V Rel y) c     = ((  x0, y+y0), (  x0, y+y0), path ++ [straight' (0,    y   )], c)

nextSegment (_, (x0,y0), path, C Abs (x1,y1,x2,y2,x,y)) c = ((x2,y2), (x,y), path ++ [bez3 (x1-x0, y1-y0) (x2-x0, y2-y0) (x-x0,y-y0)], c)
nextSegment (_, (x0,y0), path, C Rel (x1,y1,x2,y2,x,y)) c = ((x2+x0, y2+y0), (x+x0, y+y0), path ++ [bez3 (x1, y1) (x2, y2) (x,y)], c)

nextSegment ((cx,cy),(x0,y0), path, S Abs (x2,y2,x,y)) c = ((x2, y2), (x, y), path ++ [bez3 (x0-cx, y0-cy) (x2-x0, y2-y0) (x-x0, y-y0)], c)
nextSegment ((cx,cy),(x0,y0), path, S Rel (x2,y2,x,y)) c = ((x2+x0, y2+y0), (x+x0, y+y0), path ++ [bez3 (x0-cx, y0-cy) (x2, y2) (x, y)], c)

nextSegment (_, (x0,y0), path, Q Abs (x1,y1,x,y)) c = ((x1, y1),       (x, y), path ++ [bez3 (x1-x0, y1-y0) (x-x0, y-y0) (x-x0, y-y0)], c)
nextSegment (_, (x0,y0), path, Q Rel (x1,y1,x,y)) c = ((x1+x0, y1+y0), (x+x0, y+y0), path ++ [bez3 (x1, y1) (x, y) (x, y)], c)

nextSegment ((cx,cy), (x0,y0), path, T Abs (x,y)) c = ((2*x0-cx, 2*y0-cy ), (x, y), path ++
                                                       [bez3 (x0-cx, y0-cy) (x-x0, y-y0) (x-x0, y-y0)], c)
nextSegment ((cx,cy), (x0,y0), path, T Rel (x,y)) c = ((2*x0-cx, 2*y0-cy),  (x, y), path ++
                                                       [bez3 (x0-cx, y0-cy) (x, y) (x, y)], c)

nextSegment (controlPoint, startPoint, path, c0 ) c1 = traceC (controlPoint, startPoint, path, c0)

--  nextSegment (_, (x0,y0), path, A Abs (rx,ry,xAxisRot,fl0,fl1,x,y) ) c = ((0,0), (0,0), path ++
--                                                                           [svgArc rx ry xAxisRot fl0 fl1 x y], c)
--  nextSegment (_, (x0,y0), path, A Rel (rx,ry,xAxisRot,fl0,fl1,x,y) ) c = ((0,0), (0,0), path ++
--                                                                           [svgArc rx ry xAxisRot fl0 fl1 x y], c)

traceC (contr,point,path,c) = Debug.Trace.trace (show c) (contr,point,path,c)


straight' = straight . r2
bez3 point1 point2 point3 = bezier3 (r2 point1) (r2 point2) (r2 point3)

