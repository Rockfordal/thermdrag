module Components.Update where

-- import Prelude -- (void, (#), (~>))
-- import Components.Init (initialState)
-- import Components.Typer (State, Query(..), Message(..))
-- import Components.Helpers (patchcircle)
-- import Data.Array (updateAt)
-- import Data.Maybe (Maybe(..), fromMaybe)
-- import Data.Maybe (Maybe(..))
-- import Halogen as H



-- performAction :: PerformAction _ State _ Action
-- performAction KABOOM _ _ =
--   void (cotransform (\state ->
--     state { name = "Holy Cow!!! " }))

-- performAction (DragStart i { x, y }) _ _ =
--   void (cotransform (\state -> 
--       state { drag =
--         Just { current: { x: x, y: y }
--              , start:   { x: x, y: y }
--              , index: i }}))

-- performAction (DragAt {x, y}) _ _ =
--   void (cotransform (\state -> 
--     case state.drag of
--       Nothing -> state
--       Just drag ->
--         state { drag = Just drag 
--                 { current { x = x, y = y } } }))

-- performAction DragEnd _ _ =
--   void (cotransform (\state ->
--     case state.drag of
--       Nothing -> state
--       Just { index } ->
--         let
--           newcircles =
--             state.circles
--             # updateAt index (patchcircle state index)
--             # fromMaybe [stupidCircle]
--         in
--           state { circles = newcircles
--                 , drag    = Nothing }))