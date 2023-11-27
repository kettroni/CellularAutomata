module CA.Position.Point2d
 ( Point2d(..)
 , generate2dGrid
 ) where

import Prelude

import CA.Position (class Position)
import Data.Array ((..))

newtype Point2d = Point2d { x :: Int
                          , y :: Int
                          }

derive instance ordPoint2d :: Ord Point2d

instance eqPoint2d :: Eq Point2d where
  eq (Point2d p1) (Point2d p2) = p1.x == p2.x && p1.y == p2.y

instance showPosition2d :: Show Point2d where
  show (Point2d p) = show p

instance positionPoint2d :: Position Point2d where
  neighbours :: Point2d -> Array Point2d
  neighbours (Point2d p) =
    [ Point2d { x: p.x - 1, y: p.y - 1 }
    , Point2d { x: p.x,     y: p.y - 1 }
    , Point2d { x: p.x + 1, y: p.y - 1 }

    , Point2d { x: p.x - 1, y: p.y     }
    , Point2d { x: p.x + 1, y: p.y     }

    , Point2d { x: p.x - 1, y: p.y + 1 }
    , Point2d { x: p.x,     y: p.y + 1 }
    , Point2d { x: p.x + 1, y: p.y + 1 }
    ]

generate2dGrid :: Int -> Int -> Array Point2d
generate2dGrid width height = do
  x' <- 1..width
  y' <- 1..height
  pure $ Point2d { x: x', y: y' }
