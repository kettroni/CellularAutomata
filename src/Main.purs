module Main where

import Prelude

import CellularAutomata (nextState)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Gol (Gol2d, generateDefaultGol2d)

main :: Effect Unit
main = do
  logShow generateDefaultGol2d
  loop generateDefaultGol2d

loop :: Gol2d -> Effect Unit
loop s = do
  launchAff_
    do
      delay $ Milliseconds 3000.0
      liftEffect $ logShow $ nextState s
      liftEffect $ loop $ nextState s
