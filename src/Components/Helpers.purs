module Components.Helpers where

import Components.Init (defaultCircle)
import Components.Typer (Circle, State)
import Data.Array ((!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Prelude ((#), (+), (-), (==))


getcircle :: State -> Int -> Circle
getcircle state i =
  case state.drag of
    Nothing -> thiscircle
    Just { start, current, index } ->
      if index == i then
        thiscircle { x = thiscircle.x + current.x - start.x
                   , y = thiscircle.y + current.y - start.y }
      else
        thiscircle
  where
  thiscircle = state.circles !! i # fromMaybe defaultCircle
