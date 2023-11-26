module Position where

import Prelude

newtype Position = Position { x :: Int
                            , y :: Int
                            }

derive instance positionOrd :: Ord Position

instance positionEq :: Eq Position where
  eq (Position p1) (Position p2) = p1.x == p2.x && p1.y == p2.y

instance showPosition :: Show Position where
  show (Position p) = show p
