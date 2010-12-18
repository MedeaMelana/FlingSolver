{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fling where

import Control.Monad ()
import Control.Arrow (second, (***))
import Data.List (sort, groupBy, subsequences)
import Data.Function (on)
import Data.Tree
import Data.Maybe (fromJust)

type Point = (Y, X)
type Game = [Point]
data Move = Move Point Dir deriving (Eq, Show, Read)
data Dir = North | East | South | West deriving (Eq, Show, Read, Enum, Bounded)
type Row = [Int]
type X = Int
type Y = Int

dirPoints :: [(Dir, Point)]
dirPoints =
  [ (North, ((-1), 0))
  , (East, (0,1))
  , (South, (1,0))
  , (West, (0,(-1)))
  ]

type Transformation = [Point -> Point]

class Transform a where
  xfTo   :: Transformation -> a -> a
  xfFrom :: Transformation -> a -> a

instance Transform Point where
  xfTo   = foldr (.) id
  xfFrom = xfTo . reverse

instance Transform a => Transform [a] where
  xfTo   xf = map (xfTo xf)
  xfFrom xf = map (xfFrom xf)

instance Transform Move where
  xfTo   xf (Move pt dir) = Move (xfTo xf pt)   (xfTo xf dir)
  xfFrom xf (Move pt dir) = Move (xfFrom xf pt) (xfFrom xf dir)

instance Transform Dir where
  xfTo   xf = pointToDir . xfTo xf . dirToPoint
    where
      dirToPoint = fromJust . flip lookup dirPoints
      pointToDir = fromJust . flip lookup (map swap dirPoints)
      swap (x, y) = (y, x)

example0 :: Game
example0 = [(1,4),(1,5),(2,1),(2,5),(5,5),(7,2)]

example1 :: Game
example1 = [(0,0),(1,2),(2,0),(3,1),(6,3),(7,1)]

printSolutions :: Game -> IO ()
printSolutions = putStr . drawForest . (fmap . fmap) show . solutions . search

solutions :: Forest (Move, Game) -> Forest (Move, Game)
solutions = concatMap f
  where
    f n@(Node (_, [_]) []) = [n]
    f (Node _ []) = []
    f (Node mg cs) =
      case solutions cs of
        [] -> []
        cs' -> [Node mg cs']

search :: Game -> Forest (Move, Game)
search = map (\(m, g') -> Node (m, g') (search g')) . moves

-- | Noemt gegeven een state alle mogelijke zetten in die state, elk gekoppeld
-- met de bijbehorende nieuwe state.
moves :: Game -> [(Move, Game)]
moves g = concatMap f xforms
  where
    f xf    = map (xfFrom xf *** xfFrom xf)
            . (map . second) fromRows
            . shifts
            . toRows
            . map (xfTo xf)
            $ g

xforms :: [Transformation]
xforms = subsequences [mirrorX, mirrorDiag]

mirrorX :: Point -> Point
mirrorX = second negate

mirrorDiag :: Point -> Point
mirrorDiag (x,y) = (y,x)

groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

toRows :: Game -> [(Y, Row)]
toRows = map (\row -> (fst (head row), map snd row)) . groupOn fst . sort

fromRows :: [(Y, Row)] -> Game
fromRows = concatMap (\(y, row) -> map (y,) row)

-- | Probeert voor alle rijen alle bolletjes naar rechts te rollen.
shifts :: [(Y, Row)] -> [(Move, [(Y, Row)])]
shifts [] = []
shifts ((y, row) : yrows) = map (\(x, r) -> ((Move (y, x) East), (y, r) : yrows)) (shift row) ++ map (second ((y, row) :)) (shifts yrows)

-- | Probeert voor 1 rij alle balletjes naar rechts te rollen (per balletje shift1).
shift :: Row -> [(X, Row)]
shift []       = []
shift (x : xs) = maybe id (:) (shift1 (x : xs)) ((fmap . second) (x :) (shift xs))

-- | Probeert voor 1 rij het eerste balletje naar rechts te rollen.
shift1 :: Row -> Maybe (X, Row)
shift1 [] = Nothing
shift1 [_] = Nothing
shift1 (x : y : zs)
  | x + 1 == y  = Nothing
  | otherwise   = Just (x, map pred (y : zs))
