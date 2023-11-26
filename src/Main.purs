module Main where

import Prelude

import Data.Array ((..))
import Data.Map (fromFoldable, unionWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Gol (Board, Position(..), Status(..), updatedBoard)

type Options = { height :: Int , width :: Int }

defaultOptions :: Options
defaultOptions = { height: 3 , width: 3 }

emptyBoard :: Options -> Board
emptyBoard opt = fromFoldable do
  x <- 1..opt.width
  y <- 1..opt.height
  pure $ Tuple (Position { x: x, y: y }) Dead

defaultAliveCells :: Board
defaultAliveCells = fromFoldable
  [ Tuple (Position { x: 1, y: 1 }) Alive
  , Tuple (Position { x: 1, y: 2 }) Alive
  ]

collisionHelper :: Status -> Status -> Status
collisionHelper s1 s2
  | s1 == Dead && s2 == Dead = Dead
  | otherwise             = Alive

main :: Effect Unit
main = do
  log $ show $ start
  log $ show $ next
    where
      start = unionWith (collisionHelper) (emptyBoard defaultOptions) defaultAliveCells
      next = updatedBoard start
