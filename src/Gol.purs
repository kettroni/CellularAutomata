module Gol where

import Prelude

import Board (Board)
import Data.Array (filter)
import Data.Foldable (length)
import Data.Map (lookup, mapMaybeWithKey)
import Data.Maybe (Maybe(..), fromMaybe)
import Position (Position(..))
import Status (Status(..))

nextBoard :: Board -> Board
nextBoard b = mapMaybeWithKey (\ k _ -> Just $ nextStatus k b) b

nextStatus :: Position -> Board -> Status
nextStatus pos b
  | between 2 3 (aliveNeighboursCount pos b) = Alive
  | otherwise                                = Dead

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
