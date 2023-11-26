module Gol where

import Prelude

import Cell (class Cell)
import CellularAutomata (emptyState, specificState)
import Data.Map (Map, unionWith)
import Position (Point2d(..), generate2dGrid)

-- Cell
data GolCell = Dead | Alive
derive instance eqGolCell :: Eq GolCell
derive instance ordGolCell :: Ord GolCell

instance semigroupGolCell :: Semigroup GolCell where
  append Alive _ = Alive
  append _ Alive = Alive
  append _ _     = Dead

instance monoidGolCell :: Monoid GolCell where
  mempty = Dead

instance showGolCell :: Show GolCell where
  show Dead  = "Dead"
  show Alive = "Alive"

instance cellGolCell :: Cell GolCell where
  cell aliveNeighbours
    | between 2 3 aliveNeighbours = Alive
    | otherwise                   = Dead

type Gol2d = Map Point2d GolCell

defaultAliveCells :: Gol2d
defaultAliveCells = specificState Alive
  [ Point2d { x: 1, y: 1 }
  , Point2d { x: 1, y: 2 }
  ]

generateGol2d :: Int -> Int -> Gol2d
generateGol2d width height = unionWith append (emptyState $ generate2dGrid width height) defaultAliveCells

generateDefaultGol2d :: Gol2d
generateDefaultGol2d = generateGol2d 3 3
