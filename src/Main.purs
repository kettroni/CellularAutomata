module Main where

import Prelude

import CellularAutomata (nextState)
import Effect (Effect)
import Effect.Console (log)
import Gol (generateDefaultGol2d)

main :: Effect Unit
main = do
  log $ show $ generateDefaultGol2d
  log $ show $ nextState generateDefaultGol2d
