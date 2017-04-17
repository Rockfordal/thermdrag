module Components.Update where
import Components.Helpers (getcircle)
import Components.Init (defaultCircle)
import Components.Typer (Action(..), State)
import Data.Array (updateAt)
import Data.Maybe (Maybe(..), fromMaybe)
import Prelude (void, (#))
import Thermite (PerformAction, cotransform)


performAction :: PerformAction _ State _ Action
performAction Rubbe _ _ = void (cotransform (\state -> state { name = "rubbe " }))
performAction (DragStart i { x, y }) _ _ =
  void (cotransform (\state -> 
      state { drag = Just { current: { x: x, y: y }
                          , start:   { x: x, y: y }
                          , index: i }}))


performAction (DragAt {x, y}) _ _ =
  void (cotransform (\state -> 
    case state.drag of
      Nothing   -> state
      Just drag -> state { drag = Just drag { current { x = x, y = y } } }))


performAction DragEnd _ _ =
  void (cotransform (\state ->
    case state.drag of
      Nothing -> state
      Just { index } ->
        let
          newcircle  = getcircle state index
          newcircles = state.circles
                      # updateAt index newcircle
                      # fromMaybeCircle
          fromMaybeCircle = fromMaybe [defaultCircle]
        in
          state { circles = newcircles
                , drag = Nothing }))
