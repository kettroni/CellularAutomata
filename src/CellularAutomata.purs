module CellularAutomata where

import Prelude

import Cell (class Cell, cell)
import Data.Array (filter, length)
import Data.Map (Map, fromFoldable, lookup, mapMaybeWithKey)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Position (class Position, generate, neighbours)

-- Cellular Automata (CA)
class (Position p, Cell c) <= CA p c where
  nextState :: CA p c => Map p c -> Map p c

defaultEmptyState :: forall p c. CA p c => Map p c
defaultEmptyState = emptyState generate

emptyState :: forall p c. CA p c => Array p -> Map p c
emptyState = specificState mempty

specificState :: forall p c. CA p c => c -> Array p -> Map p c
specificState cell positions = fromFoldable do
  p <- positions
  pure $ Tuple p cell

defaultNextState :: forall p c. CA p c => Map p c -> Map p c
defaultNextState m = mapMaybeWithKey (\ k _ -> Just $ cell (aliveNeighboursCount m k)) m

aliveNeighboursCount :: forall p c. CA p c => Map p c -> p -> Int
aliveNeighboursCount m = length <<< filter (aliveAndFoundFromMap m) <<< neighbours

aliveAndFoundFromMap :: forall p c. CA p c => Map p c -> p -> Boolean
aliveAndFoundFromMap m position = mempty /= fromMaybe mempty (lookup position m)
