module Components.Draw where

import Halogen.HTML.Events as HE
import DOM.Event.MouseEvent (clientX, clientY)
import Data.Array (mapWithIndex, modifyAt, (..))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Halogen (Component, component, get, gets, modify)
import Halogen.Component (ComponentDSL)
import Halogen.HTML (HTML)
import Prelude (class Eq, class Ord, type (~>), Unit, Void, bind, const, discard, not, pure, unit, (#), ($), (*), (+), (-), (/), (==))
import Svg.Attributes (Color(..), Command(..), D(..), cx, cy, d, fill, r, stroke, viewBox)
import Svg.Elements (circle, g, path, svg)

derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

type DSL e = ComponentDSL State Input Void e

type State = { on :: Boolean
             , drag :: Maybe Drag
             , circles :: Array Circle
             }

type Circle =
  { r :: Int
  , x :: Int
  , y :: Int
  , c :: Color
  }

type Drag =
  { start :: Position
  , current :: Position
  , index :: Int
  }

type Position =
  { x :: Int
  , y :: Int
  }

data Input a
  = ToggleState a
  | DragStart { x :: Int, y :: Int, i :: Int } a
  | DragAt Position a
  | DragEnd Int a

data Slot = Slot

sampleCircle :: Circle
sampleCircle = { x: 100, y: 100, r: 50, c: (RGB 1 1 1) }

sampleCircles :: Array Circle
sampleCircles = do
  x <- values
  y <- values
  pure { x: mm x, y: mm y, r: 30, c: (RGB 1 (mm x) (mm y)) }
  where
  values = 1 .. 5
  mm = (\n -> 30 + n * 70 )


ui :: forall e. Component HTML Input Unit Void e
ui = component { initialState: const initial, render, eval, receiver: const Nothing }
  where
  initial = { on: false, drag: Nothing, circles: sampleCircles }

  render state =
    let
      w = 1905.0
      h = 1905.0
      x = 0.0
      y = 0.0
    in
      svg -- pageX
      [ viewBox x y w h
      , HE.onMouseMove \e -> HE.input DragAt { x: (clientX e), y: (clientY e) }
      , HE.onMouseUp   \e -> HE.input DragEnd 1
      ] [ g []
          $ state.circles # mapWithIndex (viewCircle state)
        , circle [ cx 50.0, cy 50.0, r 10.0, fill jblack ]
        , path [ d [ Abs (M 70.0 70.0), Abs (L 90.0 90.0) ]
                  , fill jblack
                  , stroke jblack
                  ]
        ]
      where jblack = Just (RGB 0 0 0)

  viewCircle state index oldcircle =
    let
      cc =
          case state.drag of
            Just drag -> patchcircle drag index oldcircle
            Nothing -> oldcircle
    in
      circle
      [ cx $ toNumber cc.x
      , cy $ toNumber cc.y
      , r $ toNumber cc.r
      , fill (Just cc.c)
      , HE.onMouseDown \e -> HE.input DragStart { x: (clientX e), y: (clientY e), i: index }
      ]

  patchcircle :: Drag -> Int -> Circle -> Circle
  patchcircle { start, current, index } i oldcircle =
    if index == i then
      oldcircle { x = oldcircle.x + current.x - start.x
                , y = oldcircle.y + current.y - start.y }
    else oldcircle


  eval :: Input ~> DSL e
  eval (ToggleState next) = do
    on <- gets _.on
    modify (_{ on = not on })
    pure next

  eval (DragStart { x, y, i } next) = do
    modify (_{ drag =
        Just { current: { x: x + addx, y: y + addy }
             , start:   { x: x + addx, y: y + addy }
             , index: i }})
    pure next
    where
    addx = 0
    addy = 0

  eval (DragAt { x, y } next) = do
    state <- get
    case state.drag of
      Nothing -> pure unit
      Just drag ->
        modify (_ { drag =
          Just drag { current { x = addx + (floor (toNumber x / modx))
                              , y = addy + (floor (toNumber y / mody)) } } })
    pure next
    where
      modx = 1.0 -- 1.8
      mody = 1.0 -- 1.5
      addx = 0 -- 600
      addy = 0 -- 200

  eval (DragEnd _ next) = do
    state <- get
    case state.drag of
      Nothing -> pure unit
      Just drag ->
        let
          newcircles =
            state.circles
            # modifyAt drag.index (patchcircle drag drag.index)
            # fromMaybe [sampleCircle]
        in
          modify (_{ circles = newcircles
                   , drag    = Nothing })
    pure next


-- x :: forall t1 t2. HTML t2 t1
-- linje :: Int -> Int -> Int -> Int -> element
-- linje x1 y1 x2 y2 =
--   line
--     [ p "x1" x1
--     , p "y1" y1
--     , p "x2" x2
--     , p "y2" y2
--     , p "stroke" "black"
--     , p "stroke-width" 2
--     ] []
--     where p = unsafeMkProps

  -- linje b1.x b1.y b2.x b2.y

  -- findcolor :: Int -> Int -> Color
  -- findcolor x y =
  --   RGB 1 x y
  --     -- colors !! x # fromMaybe brown
  --   -- toHexStringSource
  --   -- where colors = [red, green, blue, yellow, pink]

