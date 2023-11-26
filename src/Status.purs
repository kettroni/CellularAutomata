module Status where

import Data.Bounded (class Ord)
import Data.Eq (class Eq)
import Data.Show (class Show)

data Status = Dead | Alive

derive instance statusEq :: Eq Status
derive instance statusOrd :: Ord Status

instance showStatus :: Show Status where
  show Dead  = "Dead"
  show Alive = "Alive"
