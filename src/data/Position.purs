module CA.Position
  ( class Position
  , neighbours
  ) where

import Prelude

class Ord a <= Position a where
  neighbours :: a -> Array a
