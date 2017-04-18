module Components.View where

import Prelude
import Components.Common (linje)
import Components.Helpers (patchcircle)
import Components.Init (stupidCircle)
import Components.Typer (Action(..), Circle, State)
import Data.Array (mapWithIndex, (!!))
import Data.Int (floor)
import Data.Maybe (fromMaybe)
import Lib.Svg (cx, cy, fill, radius)
import React (ReactElement)
import React.DOM (h1, text, p', button)
import React.DOM.Props (className, height, onClick, onMouseDown, onMouseMove, onMouseUp, width)
import React.DOM.SVG (circle, g', svg)
import Thermite (Render)


render :: Render State _ Action
render dispatch _ state _ =
  [ h1 [ ]
    [ text $ "Hello " <> state.name ]
    , p' [ text state.name ]
    , svg [ width "800", height "600"
          , onMouseMove \e -> dispatch $ DragAt { x: e.pageX # floor
                                                , y: e.pageY # floor } 
          , onMouseUp   \e -> dispatch $ DragEnd ]
      [ linje b1.x b1.y b2.x b2.y
      , g' $ state.circles # mapWithIndex (viewcircle state dispatch) 
      ]
    , button [ className "btn btn-success"
               , onClick \_ -> dispatch KABOOM ]
                 [ text "Press it!" ]
  ]
  where
    b1 = state.circles !! 1 # fromMaybe stupidCircle
    b2 = state.circles !! 2 # fromMaybe stupidCircle


viewcircle :: State -> _ -> Int -> Circle -> ReactElement
viewcircle state dispatch index oldcircle =
    circle
    [ cx cc.x
    , cy cc.y
    , radius cc.r
    , fill cc.c
    , onMouseDown \e ->
      dispatch $ DragStart index
                           { x: e.pageX # floor
                           , y: e.pageY # floor } ] [ ] 
    where cc = patchcircle state index oldcircle

