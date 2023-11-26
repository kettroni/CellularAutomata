module Main where

import Prelude

import CellularAutomata (nextState)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Gol (Gol2d, generateDefaultGol2d)

main :: Effect Unit
main = do
  logShow generateDefaultGol2d
  launchAff_
    do
      loop generateDefaultGol2d

loop :: Gol2d -> Aff Unit
loop s = do
  delay $ Milliseconds 100.0
  liftEffect $ logShow $ nextState s
  loop $ nextState s
