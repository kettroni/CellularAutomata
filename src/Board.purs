module Board where

import Prelude

import Data.Map (Map)
import Position (Position)
import Status (Status)

type Board = Map Position Status
