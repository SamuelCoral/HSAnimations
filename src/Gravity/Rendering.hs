module Gravity.Rendering where

import Control.Lens
import Graphics.Gloss
import Math.Types
import Gravity.Types



renderGameState :: GameState -> Picture
renderGameState (GameState [_, _, Point2D x y]) = Circle 100
        & Color red
        & Translate x y

