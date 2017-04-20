module Components.View where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Components.Typer (State, Query(..))
-- import Components.Init (initialState)
-- import Components.Common (linje)
-- import Components.Helpers (patchcircle)
-- import Data.Array (mapWithIndex, (!!))
-- import Data.Int (floor)
-- import Lib.Svg (cx, cy, fill, radius)


render :: State -> H.ComponentHTML Query
render state =
    let
        label = if state then "On" else "off"
    in
        HH.div_
          [ HH.h1_ [ HH.text "Hello there" ]
          , HH.button
            [ HP.title label
            , HP.class_ (H.ClassName "btn btn-success")
            , HE.onClick (HE.input_ Toggle)
            ]
            [ HH.text label ]
          ]


-- render :: Render State _ Action
-- render dispatch _ state _ =
--   [ h1 [ ]
--     [ text $ "Hello " <> state.name ]
--     , p' [ text state.name ]
--     , svg [ width "800", height "600"
--           , onMouseMove \e -> dispatch $ DragAt { x: e.pageX # floor
--                                                 , y: e.pageY # floor } 
--           , onMouseUp   \e -> dispatch $ DragEnd ]
--       [ linje b1.x b1.y b2.x b2.y
--       , g' $ state.circles # mapWithIndex (viewcircle state dispatch) 
--       ]
--     , button [ className "btn btn-success"
--                , onClick \_ -> dispatch KABOOM ]
--                  [ text "Press it!" ]
--   ]
--   where
--     b1 = state.circles !! 1 # fromMaybe stupidCircle
--     b2 = state.circles !! 2 # fromMaybe stupidCircle


-- viewcircle :: State -> _ -> Int -> Circle -> ReactElement
-- viewcircle state dispatch index oldcircle =
--     circle
--     [ cx cc.x
--     , cy cc.y
--     , radius cc.r
--     , fill cc.c
--     , onMouseDown \e ->
--       dispatch $ DragStart index
--                            { x: e.pageX # floor
--                            , y: e.pageY # floor } ] [ ] 
--     where cc = patchcircle state index oldcircle

