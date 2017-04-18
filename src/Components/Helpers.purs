module Components.Helpers where

import Prelude ((#), (+), (-), (==))
import Components.Init (stupidCircle)
import Components.Typer (Circle, State)
import Data.Array ((!!))
import Data.Maybe (Maybe(..), fromMaybe)


patchcircle :: State -> Int -> Circle
patchcircle state i =
  case state.drag of
    Nothing -> circle
    Just { start, current, index } ->
      if index == i then
        circle { x = circle.x + current.x - start.x
               , y = circle.y + current.y - start.y }
      else
        circle
  where
  circle = state.circles !! i # fromMaybe stupidCircle
