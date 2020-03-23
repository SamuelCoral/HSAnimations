module Lorenz.Main where

import Graphics.Gloss
import Lorenz.Types
import Lorenz.Rendering
import Lorenz.TimeHandling


lorenzAnimation :: IO ()
lorenzAnimation = simulate
    (InWindow ":v" (800, 600) (200, 100))
    black
    60
    lorenzSystem
    renderLorenz
    stepLorenz
