module CellularAutomata where

import Prelude

import Cell (class Cell, cell)
import Data.Array (filter, length)
import Data.Map (Map, fromFoldable, lookup, mapMaybeWithKey)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Position (class Position, generate, neighbours)

defaultEmptyState :: forall p c. Position p => Cell c => Map p c
defaultEmptyState = emptyState generate

emptyState :: forall p c. Position p => Cell c => Array p -> Map p c
emptyState = specificState mempty

specificState :: forall p c. Position p => Cell c => c -> Array p -> Map p c
specificState cell positions = fromFoldable do
  p <- positions
  pure $ Tuple p cell

nextState :: forall p c. Position p => Cell c => Map p c -> Map p c
nextState m = mapMaybeWithKey (\ k _ -> Just $ cell (aliveNeighboursCount m k)) m

aliveNeighboursCount :: forall p c. Position p => Cell c => Map p c -> p -> Int
aliveNeighboursCount m = length <<< filter (aliveAndFoundFromMap m) <<< neighbours

aliveAndFoundFromMap :: forall p c. Position p => Cell c => Map p c -> p -> Boolean
aliveAndFoundFromMap m position = mempty /= fromMaybe mempty (lookup position m)
