module Lorenz.Rendering where

import Math.Types
import Math.Quaternion
import Math.Gloss
import Math.Projections
import Graphics.Gloss
import Lorenz.Types
import Control.Lens


buildLorenzPath :: [Color] -> Path -> Picture 
buildLorenzPath (c:cs) (p1:p2:ps) = Color c $ Line [p1, p2] <> buildLorenzPath cs (p2:ps)
buildLorenzPath _ _ = mempty


renderLorenz :: Lorenz Float -> Picture
renderLorenz l = buildLorenzPath
    (l ^. colors) $
    toListOf (each.glossPoint) $
    projectCurve3D 300 (l^.location) (exp $ l^.rotationAxis) (l^.path)
