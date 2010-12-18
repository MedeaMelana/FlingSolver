{-# LANGUAGE TupleSections #-}

module Fling where

import Control.Monad ()
import Control.Arrow (second, (***))
import Data.List (sort, groupBy, subsequences)
import Data.Function (on)
import Data.Tree

type Point = (Y, X)
type GameState = [Point]
data Move = Move Point Dir deriving Show
type Dir = Point
type Row = [Int]
type X = Int
type Y = Int

example0 :: GameState
example0 = [(1,4),(1,5),(2,1),(2,5),(5,5),(7,2)]

example1 :: GameState
example1 = [(0,0),(1,2),(2,0),(3,1),(6,3),(7,1)]

printSolutions :: GameState -> IO ()
printSolutions = putStr . drawForest . (fmap . fmap) show . solutions . search

solutions :: Forest (Move, GameState) -> Forest (Move, GameState)
solutions = concatMap f
  where
    f n@(Node (_, [_]) []) = [n]
    f (Node _ []) = []
    f (Node mg cs) =
      case solutions cs of
        [] -> []
        cs' -> [Node mg cs']

search :: GameState -> Forest (Move, GameState)
search = map (\(m, g') -> Node (m, g') (search g')) . moves

-- | Noemt gegeven een state alle mogelijke zetten in die state, elk gekoppeld
-- met de bijbehorende nieuwe state.
moves :: GameState -> [(Move, GameState)]
moves g = concatMap f xforms
  where
    f xform = map ((\(Move pt dir) -> Move (xformFrom xform pt) (xformFrom xform dir)) *** map (xformFrom xform))
            . (map . second) fromRows
            . shifts
            . toRows
            . map (xformTo xform)
            $ g

xforms :: [[Point -> Point]]
xforms = subsequences [mirrorX, mirrorDiag]

xformTo :: [Point -> Point] -> Point -> Point
xformTo = foldr (.) id

xformFrom :: [Point -> Point] -> Point -> Point
xformFrom = xformTo . reverse

mirrorX :: Point -> Point
mirrorX = second negate

mirrorDiag :: Point -> Point
mirrorDiag (x,y) = (y,x)

groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

toRows :: GameState -> [(Y, Row)]
toRows = map (\row -> (fst (head row), map snd row)) . groupOn fst . sort

fromRows :: [(Y, Row)] -> GameState
fromRows = concatMap (\(y, row) -> map (y,) row)

-- | Probeert voor alle rijen alle bolletjes naar rechts te rollen.
shifts :: [(Y, Row)] -> [(Move, [(Y, Row)])]
shifts [] = []
shifts ((y, row) : yrows) = map (\(x, r) -> ((Move (y, x) (0, 1)), (y, r) : yrows)) (shift row) ++ map (second ((y, row) :)) (shifts yrows)

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
