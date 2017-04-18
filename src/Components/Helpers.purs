module Components.Helpers where

import Prelude ((+), (-), (==))
import Components.Typer (Circle, State)
import Data.Maybe (Maybe(Nothing, Just))


patchcircle :: State -> Int -> Circle -> Circle
patchcircle state i oldcircle =
  case state.drag of
    Nothing -> oldcircle
    Just { start, current, index } ->
      if index == i then
        oldcircle { x = oldcircle.x + current.x - start.x
                  , y = oldcircle.y + current.y - start.y }
      else
        oldcircle
