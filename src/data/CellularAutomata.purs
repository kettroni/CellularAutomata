-- Cellular Automata (CA)
module CA
  ( class CellularAutomata
  , nextState
  , defaultNextState
  , emptyState
  , specificState) where

import Prelude

import CA.Cell (class Cell, cell)
import CA.Gol.Cell (GolCell)
import CA.Position (class Position, neighbours)
import CA.Position.Point2d (Point2d)
import Data.Array (filter, length)
import Data.Map (Map, fromFoldable, lookup, mapMaybeWithKey)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))

class (Position p, Cell c) <= CellularAutomata p c where
  nextState :: CellularAutomata p c => Map p c -> Map p c

emptyState :: forall p c. CellularAutomata p c => Array p -> Map p c
emptyState = specificState mempty

specificState :: forall p c. CellularAutomata p c => c -> Array p -> Map p c
specificState cell positions = fromFoldable do
  p <- positions
  pure $ Tuple p cell

defaultNextState :: forall p c. CellularAutomata p c => Map p c -> Map p c
defaultNextState m = mapMaybeWithKey (\ k _ -> Just $ cell (aliveNeighboursCount m k)) m

aliveNeighboursCount :: forall p c. CellularAutomata p c => Map p c -> p -> Int
aliveNeighboursCount m = length <<< filter (aliveAndFoundFromMap m) <<< neighbours

aliveAndFoundFromMap :: forall p c. CellularAutomata p c => Map p c -> p -> Boolean
aliveAndFoundFromMap m position = mempty /= fromMaybe mempty (lookup position m)

instance caGol2d :: CellularAutomata Point2d GolCell where
  nextState = defaultNextState
