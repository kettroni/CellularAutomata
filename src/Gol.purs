module Gol where

import Prelude

import Data.Array (filter)
import Data.Foldable (length)
import Data.Map (Map, lookup, mapMaybeWithKey)
import Data.Maybe (Maybe(..), fromMaybe)

type Board = Map Position Status

newtype Position = Position { x :: Int
                            , y :: Int
                            }
instance positionEq :: Eq Position where
  eq (Position p1) (Position p2) = p1.x == p2.x && p1.y == p2.y
derive instance positionOrd :: Ord Position
instance showPosition :: Show Position where
  show (Position p) = show p

data Status = Dead | Alive
derive instance statusEq :: Eq Status
derive instance statusOrd :: Ord Status
instance showStatus :: Show Status where
  show Dead = "Dead"
  show Alive = "Alive"

updatedBoard :: Board -> Board
updatedBoard b = mapMaybeWithKey (\ k _ -> Just $ updatedStatus k b) b

updatedStatus :: Position -> Board -> Status
updatedStatus pos b
  | aliveNeighboursCount pos b > 3 || aliveNeighboursCount pos b < 2 = Dead
  | otherwise                                                       = Alive

aliveNeighboursCount :: Position -> Board -> Int
aliveNeighboursCount point b = length $ filter (\ p -> fromMaybe Dead (lookup p b) == Alive) $ neighboursPositions point

neighboursPositions :: Position -> Array Position
neighboursPositions (Position p) =
  [ Position { x: p.x - 1, y: p.y - 1 }
  , Position { x: p.x,     y: p.y - 1 }
  , Position { x: p.x + 1, y: p.y - 1 }

  , Position { x: p.x - 1, y: p.y     }
  , Position { x: p.x + 1, y: p.y     }

  , Position { x: p.x - 1, y: p.y + 1 }
  , Position { x: p.x,     y: p.y + 1 }
  , Position { x: p.x + 1, y: p.y + 1 }
  ]
