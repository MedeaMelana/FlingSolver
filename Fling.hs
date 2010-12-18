{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fling where

import Control.Arrow (second, (***))
import Data.List (sort, groupBy, subsequences)
import Data.Function (on)
import Data.Tree (Tree(..), Forest, drawForest)
import Data.Maybe (fromJust)


-- Example puzzles

example0 :: Game
example0 = [(1,4),(1,5),(2,1),(2,5),(5,5),(7,2)]

example1 :: Game
example1 = [(0,0),(1,2),(2,0),(3,1),(6,3),(7,1)]

example2 :: Game
example2 = [(1,4),(6,4),(7,4)]

puzzle9_1 :: Game
puzzle9_1 = [(0,0),(0,2),(1,6),(2,6),(4,1),(5,2),(6,5),(7,3)]


-- Some types

type Point = (Y, X)
type X     = Int
type Y     = Int

type Row = [X]

type Game = [Point]

data Move = Move Point Dir
  deriving (Eq, Show, Read)

data Dir = North | East | South | West
  deriving (Eq, Show, Read, Enum, Bounded)


-- Transforming stuff

type Transformation = [Point -> Point]

class Transform a where
  transform :: Transformation -> a -> a

instance Transform Point where
  transform = foldr (.) id

instance Transform a => Transform [a] where
  transform xf = map (transform xf)

instance Transform Move where
  transform xf (Move pt dir) = Move (transform xf pt) (transform xf dir)

instance Transform Dir where
  transform xf = pointToDir . transform xf . dirToPoint
    where
      dirToPoint = fromJust . flip lookup dirPoints
      pointToDir = fromJust . flip lookup (map swap dirPoints)
      swap (x, y) = (y, x)

      dirPoints :: [(Dir, Point)]
      dirPoints =
        [ (North, (-1,0))
        , (East,  (0, 1))
        , (South, (1, 0))
        , (West,  (0,-1))
        ]

transformations2D :: [Transformation]
transformations2D = subsequences [mirrorX, mirrorDiag]

mirrorX :: Point -> Point
mirrorX = second negate

mirrorDiag :: Point -> Point
mirrorDiag (x,y) = (y,x)


-- Top-level API

-- | Print a neat forest of winning strategies.
printSolutions :: Game -> IO ()
printSolutions = putStr . drawForest . (fmap . fmap) show . solutions . search

-- | Prune a game tree to only keep the winning paths.
solutions :: Forest (Move, Game) -> Forest (Move, Game)
solutions = concatMap f
  where
    -- One furball left: we have a winning position.
    f n@(Node (_, [_]) []) = [n]

    -- Multiple furballs left: recurse.
    f (Node mg cs) =
      case solutions cs of
        [] -> []
        cs' -> [Node mg cs']

-- | Build move tree from a starting position.
search :: Game -> Forest (Move, Game)
search = map (\(m, g') -> Node (m, g') (search g')) . moves


-- Generating moves

-- | Noemt gegeven een state alle mogelijke zetten in die state, elk gekoppeld
-- met de bijbehorende nieuwe state.
moves :: Game -> [(Move, Game)]
moves g = concatMap f transformations2D
  where
    f xf = map (transform (reverse xf) *** transform (reverse xf))
         . (map . second) fromRows
         . shifts
         . toRows
         . map (transform xf)
         $ g

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

toRows :: Game -> [(Y, Row)]
toRows = map (\row -> (fst (head row), map snd row)) . groupOn fst . sort

fromRows :: [(Y, Row)] -> Game
fromRows = concatMap (\(y, row) -> map (y,) row)

-- | Probeert voor alle rijen alle bolletjes naar rechts te rollen.
shifts :: [(Y, Row)] -> [(Move, [(Y, Row)])]
shifts [] = []
shifts ((y, row) : yrows) =
  map (\(x, r) -> (Move (y, x) East, (y, r) : yrows)) (shift row) ++
  map (second ((y, row) :)) (shifts yrows)

-- | Probeert voor 1 rij alle balletjes naar rechts te rollen (per balletje shift1).
shift :: Row -> [(X, Row)]
shift []       = []
shift (x : xs) = maybe id (:) (shift1 (x : xs)) ((fmap . second) (x :) (shift xs))

-- | Probeert voor 1 rij het eerste balletje naar rechts te rollen.
shift1 :: Row -> Maybe (X, Row)
shift1 (x : y : zs)
  | x + 1 == y  = Nothing
  | otherwise   = Just (x, map pred (y : zs))
shift1 _        = Nothing
