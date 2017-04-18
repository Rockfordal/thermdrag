module Components.Init where

import Prelude ((#), (+), (*), bind, pure)
import Components.Typer (State, Circle)
import Data.Array ((!!), (..))
import Data.Maybe (Maybe(Nothing), fromMaybe)


initialState :: State
initialState =
  { name: "."
  , drag: Nothing
  , circles: sampleCircles
  }


sampleCircles :: Array Circle
sampleCircles = do
  x <- values
  y <- values
  pure { x: mm x, y: mm y, r: 30, c: findcolor x }
  where
  colors = ["red", "green", "blue", "yellow", "pink"]
  values = 1 .. 5
  mm = (\n -> 30 + n * 70 ) 
  findcolor x = colors !! x # fromMaybe "black"


stupidCircle :: Circle
stupidCircle = { x:1, y:1, r:1, c: "white" } 

