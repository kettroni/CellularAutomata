module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Gol (nextBoard)
import Utils (defaultBoard)

main :: Effect Unit
main = do
  log $ show $ defaultBoard
  log $ show $ next
    where
      next = nextBoard defaultBoard
