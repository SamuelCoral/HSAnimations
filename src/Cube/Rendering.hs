module Cube.Rendering where

import Math.Types
import Math.Projections
import Math.Quaternion
import Math.Gloss
import Cube.Types
import Graphics.Gloss
import Control.Lens


buildCube :: [Point2D Float] -> Picture
buildCube ps' =
    Color red (Polygon $ take 4 ps) <>
    Color green (Polygon $ drop 4 ps) <>
    mconcat [ Color white $ Line [ps !! i, ps !! (i + 4)] | i <- [0..3] ]
    where ps = ps' ^.. each.glossPoint


renderCube :: Float -> Picture
renderCube t = buildCube
    [ project3D 500 $ view vectorPart $ initialCube^.centerCube + conjMult cubePoint
        (exp (t *. initialCube^.rotationAxisCube) * exp (Quaternion 0 0.2 0 0))
        | cubePoint <- initialCube^.verticesCube ]
