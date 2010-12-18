{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fling where

import Control.Arrow (second, (&&&))
import Data.List (sort, groupBy)
import Data.Function (on)
import Data.Tree (Tree(..), Forest, drawForest)


-- Example puzzles

example0 :: Game
example0 = mkGame [[1,4],[1,5],[2,1],[2,5],[5,5],[7,2]]

example1 :: Game
example1 = mkGame [[0,0],[1,2],[2,0],[3,1],[6,3],[7,1]]

example2 :: Game
example2 = [(1,4),(6,4),(7,4)]

puzzle9_1 :: Game
puzzle9_1 = mkGame [[0,0],[0,2],[1,6],[2,6],[4,1],[5,2],[6,5],[7,3]]


-- Some types

type Point = [Int]
type X     = Int
type Y     = [Int] -- Dimensions other than X, needs a better name.

newtype Furball = Furball { unFurball :: Point }
  deriving Eq

type Row = [X]

type Game = [Furball]

data Move = Move Furball Dir
  deriving (Eq, Show)

newtype Dir = Dir { unDir :: Point }
  deriving (Eq)

instance Show Furball where
  show (Furball pt) = show pt
  
instance Show Dir where
  show (Dir [ 0, -1]) = "North"
  show (Dir [ 1,  0]) = "East"
  show (Dir [ 0,  1]) = "South"
  show (Dir [-1,  0]) = "West"

  show (Dir [ 1,  0,  0]) = "Right"
  show (Dir [-1,  0,  0]) = "Left"
  show (Dir [ 0,  1,  0]) = "Up"
  show (Dir [ 0, -1,  0]) = "Down"
  show (Dir [ 0,  0,  1]) = "Forwards"
  show (Dir [ 0,  0, -1]) = "Backwards"
  
  show (Dir vec) = show vec

-- Transforming stuff

type Transformation = [Point -> Point]

class Transform a where
  transform :: Transformation -> a -> a

instance Transform Furball where
  transform xf = Furball . foldr (.) id xf . unFurball

instance Transform Dir where
  transform xf = Dir     . foldr (.) id xf . unDir

instance Transform a => Transform [a] where
  transform xf = map (transform xf)

instance Transform Move where
  transform xf (Move pt dir) = Move (transform xf pt) (transform xf dir)

instance (Transform a, Transform b) => Transform (a, b) where
  transform xf (a, b) = (transform xf a, transform xf b)


transformations :: Int -> [(Transformation, Transformation)]
transformations n = zip
  [          mx ++ md  | mx <- [[], [mirrorX]], md <- take n $ iterate (   mirrorDiag :) []]
  [ reverse (mx ++ md) | mx <- [[], [mirrorX]], md <- take n $ iterate (revMirrorDiag :) []]

mirrorX :: Point -> Point
mirrorX [] = []
mirrorX (x:dims) = -x:dims

mirrorDiag :: Point -> Point
mirrorDiag [] = []
mirrorDiag (x:dims) = dims ++ [x]

revMirrorDiag :: Point -> Point
revMirrorDiag [] = []
revMirrorDiag dims = last dims : init dims


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

-- | Make a game from a list of coordinates.
mkGame :: [Point] -> Game
mkGame = map Furball

-- Generating moves

-- | Noemt gegeven een state alle mogelijke zetten in die state, elk gekoppeld
-- met de bijbehorende nieuwe state.
moves :: Game -> [(Move, Game)]
moves g = concatMap f (transformations n)
  where
    n    = length . unFurball . head $ g
    f xf = transform (snd xf)
         . (map . second) fromRows
         . shifts
         . toRows
         . transform (fst xf)
         $ g

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

toRows :: Game -> [(Y, Row)]
toRows = map (\row -> (fst (head row), map snd row)) . groupOn fst . sort . map ((tail &&& head) . unFurball)

fromRows :: [(Y, Row)] -> Game
fromRows = concatMap (\(y, row) -> map (Furball . (:y)) row)

-- | Probeert voor alle rijen alle bolletjes naar rechts te rollen.
shifts :: [(Y, Row)] -> [(Move, [(Y, Row)])]
shifts [] = []
shifts ((y, row) : yrows) =
  map (\(x, r) -> (mkMove x y, (y, r) : yrows)) (shift row) ++
  map (second ((y, row) :)) (shifts yrows)
    where
      mkMove x dims = Move (Furball $ x : dims) (Dir $ 1 : replicate (length dims) 0)

-- | Probeert voor 1 rij alle balletjes naar rechts te rollen (per balletje shift1).
shift :: Row -> [(X, Row)]
shift []       = []
shift [x,y] | x + 2 == y && y <= 0 = []
shift (x : xs) = maybe id (:) (shift1 (x : xs)) ((fmap . second) (x :) (shift xs))

-- | Probeert voor 1 rij het eerste balletje naar rechts te rollen.
shift1 :: Row -> Maybe (X, Row)
shift1 (x : y : [])
  | x + 1 == y      = Nothing
  | otherwise       = Just (x, [pred y])
shift1 (x : y : zs) = Just (x, map pred (y : zs))
shift1 _            = Nothing

