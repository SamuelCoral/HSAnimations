module Lorenz.TimeHandling where

import Lorenz.Types
import Math.Types
import Math.Quaternion
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Control.Lens


rotationSpeed :: Floating a => a
rotationSpeed = 0.15

timeSpeed :: Floating a => a
timeSpeed = 0.5

stepLorenz :: ViewPort -> Float -> Lorenz Float -> Lorenz Float
stepLorenz _ t l = l &~ do

    let p = l^.path.to head
        lastColor = l^.colors.to head

    path %= cons (p + (t * timeSpeed) *. Quaternion 0
        (l^.sigma*(p^.yq - p^.xq))
        (p^.xq*(l^.rho - p^.zq) - p^.yq)
        (p^.xq*p^.yq - l^.beta*p^.zq))

    colors %= cons lastColor

    rotationAxis += Quaternion 0 0 0 (rotationSpeed * t)
