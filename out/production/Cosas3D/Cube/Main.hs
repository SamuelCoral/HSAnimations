module Cube.Main where

import Cube.Types
import Cube.Rendering
import Graphics.Gloss


cubeAnimation :: IO ()
cubeAnimation = animate
    (InWindow ":v" (800, 600) (200, 100))
    black
    renderCube
