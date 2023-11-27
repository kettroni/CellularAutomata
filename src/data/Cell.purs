module CA.Cell
  ( class Cell
  , cell
  ) where

import Prelude

class (Show a, Monoid a, Eq a) <= Cell a where
  -- Function that generates a cell given the amount of neighbours.
  cell :: Int -> a
