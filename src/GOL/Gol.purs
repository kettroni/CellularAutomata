module CA.Gol
  ( Gol2d
  , generateDefaultGol2d
  ) where

import Prelude

import CA (emptyState, specificState)
import CA.Gol.Cell (GolCell(..))
import CA.Position.Point2d (Point2d(..), generate2dGrid)
import Control.Alternative (guard)
import Data.Array ((..))
import Data.Map (Map, unionWith)

type Gol2d = Map Point2d GolCell

defaultAliveCells :: Int -> Int -> Gol2d
defaultAliveCells width height = specificState Alive
  do
    x <- 1..width
    y <- 1..height
    guard $ x == y
    pure $ Point2d { x: x, y: y }

generateGol2d :: Int -> Int -> Gol2d
generateGol2d width height = unionWith append (emptyState $ generate2dGrid width height) (defaultAliveCells width height)

generateDefaultGol2d :: Gol2d
generateDefaultGol2d = generateGol2d 3 3
