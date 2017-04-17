module Components.View where

import Prelude
import React.DOM as R
import React.DOM.SVG as S
import Thermite as T
import Components.Common (linje)
import Components.Helpers (getcircle)
import Components.Init (defaultCircle)
import Components.Typer (Action(..), State, Circle)
import Data.Array (mapWithIndex, (!!))
import Data.Int (floor)
import Data.Maybe (fromMaybe)
import Lib.Svg (cx, cy, fill, radius)
import React (ReactElement)
import React.DOM.Props (className, height, onClick, onMouseDown, onMouseMove, onMouseUp, width)


render :: T.Render State _ Action
render dispatch _ state _ =
  [ R.h1 [ ]
    [ R.text $ "Hej " <> state.name ]
    , R.p' [ R.text state.name ]
    , S.svg [ width "800", height "600"
          , onMouseMove \e -> dispatch $ DragAt { x: e.pageX # floor
                                                , y: e.pageY # floor } 
          , onMouseUp   \e -> dispatch $ DragEnd ]
      [ linje b1.x b1.y b2.x b2.y
      , S.g' $ state.circles # mapWithIndex (viewcircle state dispatch) 
      ]
    , R.button [ className "btn btn-success"
               , onClick \_ -> dispatch Rubbe ]
                 [ R.text "Tryck då!" ]
  ]
  where
    b1 = state.circles !! 1 # fromMaybe defaultCircle
    b2 = state.circles !! 2 # fromMaybe defaultCircle


viewcircle :: State -> _ -> Int -> Circle -> ReactElement
viewcircle state dispatch index circle =
    S.circle
    [ cx cc.x
    , cy cc.y
    , radius cc.r
    , fill cc.c
    , onMouseDown \e ->
      dispatch $ DragStart index { x: e.pageX # floor
                                 , y: e.pageY # floor }
    ] [ ] 
    where cc = getcircle state index

