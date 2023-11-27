module CA.Gol.Cell
  ( GolCell(..)
  ) where

import Prelude

import CA.Cell (class Cell)

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
