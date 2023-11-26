module Gol where

import Prelude

import Cell (class Cell)
import CellularAutomata (class CA, defaultNextState, emptyState, specificState)
import Control.Alternative (guard)
import Data.Array ((..))
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
instance caGol2d :: CA Point2d GolCell where
  nextState = defaultNextState

defaultAliveCells :: Int -> Int -> Gol2d
defaultAliveCells width height = specificState Alive
  do
    x <- 1..width
    y <- 1..height
    guard $ x == y
    pure $ Point2d { x: x, y: y }

generateGol2d :: Int -> Int -> Gol2d
generateGol2d width height = unionWith append (emptyState $ generate2dGrid width height) (defaultAliveCells width height)

generateDefaultGol2d :: Gol2d
generateDefaultGol2d = generateGol2d 3 3
