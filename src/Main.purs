module Main where

import Prelude

import CA (nextState)
import CA.Gol (Gol2d, generateDefaultGol2d)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (logShow)

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
