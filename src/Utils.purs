module Utils where

import Prelude

import Board (Board)
import Data.Array ((..))
import Data.Map (fromFoldable, unionWith)
import Data.Tuple (Tuple(..))
import Position (Position(..))
import Status (Status(..))

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

keepAliveStatus :: Status -> Status -> Status
keepAliveStatus s1 s2
  | s1 == Dead && s2 == Dead = Dead
  | otherwise             = Alive

defaultBoard :: Board
defaultBoard = unionWith (keepAliveStatus) (emptyBoard defaultOptions) defaultAliveCells
