{-# LANGUAGE TemplateHaskell #-}
module Cube.Types where

import Math.Types
import Math.Quaternion
import Graphics.Gloss
import Control.Lens


data Cube a = Cube {
    _verticesCube        :: [Quaternion a],
    _centerCube          :: Quaternion a,
    _rotationAxisCube    :: Quaternion a
}

$(makeLenses ''Cube)



initialCube :: Floating a => Cube a 
initialCube = Cube {
    _verticesCube        = fromPoint <$> ps ++ (ps & each . zp3 .~ (-1)),
    _centerCube          = Quaternion 0 5 0 0,
    _rotationAxisCube    = Quaternion 0 0 0 1
} where ps = [Point3D 1 1 1, Point3D (-1) 1 1, Point3D (-1) (-1) 1, Point3D 1 (-1) 1]
