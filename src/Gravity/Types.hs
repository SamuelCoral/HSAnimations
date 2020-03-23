{-# LANGUAGE TemplateHaskell #-}
module Gravity.Types where

import Control.Lens
import Graphics.Gloss
import Math.Types



newtype GameState = GameState {
    _vector1 :: [Point2D Float]
}

$(makeLenses ''GameState)



updateVector :: Num a => a -> [a] -> [a]
updateVector t = scanl1 ((+) . (* t))

