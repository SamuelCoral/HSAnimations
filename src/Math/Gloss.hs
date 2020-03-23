module Math.Gloss where

import Control.Lens
import Graphics.Gloss
import Math.Types


glossPoint :: Iso' (Point2D Float) Point
glossPoint = iso (\ (Point2D x y) -> (x, y)) $ uncurry Point2D 
