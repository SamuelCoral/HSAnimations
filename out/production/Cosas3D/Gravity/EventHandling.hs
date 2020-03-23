module Gravity.EventHandling where

import Control.Lens
import qualified Graphics.Gloss.Interface.IO.Interact as Evt
import Math.Types
import Gravity.Types



velocity :: Num a => a
velocity = 100


updateGameStateEvents :: Evt.Event -> GameState -> GameState
updateGameStateEvents evt gs = case evt of
    Evt.EventKey key keyState keyMod (x, y) -> case key of
        Evt.Char ckey -> case ckey of
            'w' -> gs & vector1 . _head +~ velocity * case keyState of
                Evt.Down    -> Point2D 0    1
                Evt.Up      -> Point2D 0    (-1)
            'a' -> gs & vector1 . _head +~ velocity * case keyState of
                Evt.Down    -> Point2D (-1) 0
                Evt.Up      -> Point2D 1    0
            's' -> gs & vector1 . _head +~ velocity * case keyState of
                Evt.Down    -> Point2D 0    (-1)
                Evt.Up      -> Point2D 0    1
            'd' -> gs & vector1 . _head +~ velocity * case keyState of
                Evt.Down    -> Point2D 1    0
                Evt.Up      -> Point2D (-1) 0
            _ -> gs
        _ -> gs
    _ -> gs

