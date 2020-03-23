module Gravity.Main where

import Gravity.Types
import Gravity.Rendering
import Gravity.EventHandling
import Gravity.TimeHandling
import Math.Types
import Graphics.Gloss


gravityGame :: IO ()
gravityGame = play
    (InWindow ":v" (800, 600) (200, 100))
    black
    30
    (GameState [Point2D 0 0, Point2D 0 0, Point2D 0 0])
    renderGameState
    updateGameStateEvents
    updateGameStateTime
