module Cell where

import Prelude

type NumberOfNeighbours = Int

class (Show a, Monoid a, Eq a) <= Cell a where
  cell :: NumberOfNeighbours -> a
