module Components.Init where

import Components.Typer (State, Circle)
import Data.Maybe (Maybe(Nothing))


initialState :: State
initialState =
  { name: "."
  , drag: Nothing
  -- , connections: [  ]
  , circles: [ { x: 30,  y: 50, r: 30, c: "red" }
             , { x: 100, y: 50, r: 30, c: "blue" }
             , { x: 170, y: 50, r: 30, c: "green" }
             , { x: 240, y: 50, r: 30, c: "yellow" }
             ] }

defaultCircle :: Circle
defaultCircle = { x:1, y:1, r:1, c: "white" } 

