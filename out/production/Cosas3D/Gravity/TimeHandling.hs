module Gravity.TimeHandling where

import Control.Lens
import Math.Types
import Gravity.Types


updateGameStateTime :: Float -> GameState -> GameState
updateGameStateTime t gs = gs & vector1 %~ updateVector (Point2D t t)

