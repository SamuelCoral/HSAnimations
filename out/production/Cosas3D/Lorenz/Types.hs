{-# LANGUAGE TemplateHaskell #-}
module Lorenz.Types where

import Math.Quaternion
import Graphics.Gloss
import Control.Lens


data Lorenz a = Lorenz {
    _rho            :: a,
    _sigma          :: a,
    _beta           :: a,
    _path           :: [Quaternion a],
    _colors         :: [Color],
    _location       :: Quaternion a,
    _rotationAxis   :: Quaternion a
}

$(makeLenses ''Lorenz)


lorenzSystem :: Floating a => Lorenz a
lorenzSystem = Lorenz {
    _rho = 28,
    _sigma = 10,
    _beta = 8/3,
    _path = [Quaternion 0 1 1 1, Quaternion 0 1.001 1.001 1.001],
    _colors = [white],
    _location = Quaternion 0 65 0 (-30),
    _rotationAxis = Quaternion 0 0 0 (pi/2)
}
