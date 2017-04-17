module Components.Helpers where

import Components.Init (defaultCircle)
import Components.Typer (Circle, State)
import Data.Array ((!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Prelude ((#), (+), (-), (==))


getcircle :: State -> Int -> Circle
getcircle state this =
  case state.drag of
    Nothing -> state.circles !! this # fromMaybe defaultCircle
    Just { start, current, index } ->
      let
        cirkel = state.circles !! this # fromMaybe defaultCircle
        x = if this == index then cirkel.x + current.x - start.x else cirkel.x
        y = if this == index then cirkel.y + current.y - start.y else cirkel.y
      in
        cirkel { x = x, y = y }

