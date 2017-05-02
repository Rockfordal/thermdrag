module Components.Draw where

-- import Svg.Attributes as SA
-- import Svg.Elements as SE
-- import Data.Maybe (Maybe(..))
-- import Halogen (Component, component, modify)
-- import Halogen.Component (ComponentDSL, ComponentHTML)
-- import Halogen.HTML (HTML(..))
-- import Halogen.HTML.Events as HE
-- import Prelude (class Eq, class Ord, type (~>), Unit, Void, bind, const, discard, negate, not, pure, unit, ($), (/))

-- derive instance eqSlot  :: Eq Slot
-- derive instance ordSlot :: Ord Slot


-- type State = { on :: Boolean }

-- data Input a
--   = ToggleState a

-- data Slot = Slot


-- ui :: forall g. Component HTML Input Unit Void g
-- ui = component { initial, render, eval, receiver: const Nothing }
--   where
--   initial = const { on: false }

--   render :: State -> ComponentHTML Input
--   render state =
--     SE.svg [SA.viewBox x y w h]
--     [ SE.circle
--       [ SA.r (if state.on then w/6.0 else w/3.0)
--       , SA.fill $ Just (SA.RGB 0 0 100)
--       , HE.onClick (HE.input_ ToggleState)
--       ]
--     ]
--     where
--     h = 150.0
--     w = 150.0
--     x = -(w / 2.0)
--     y = -(h / 2.0)

--   eval :: Input ~> ComponentDSL State Input Void g
--   eval (ToggleState next) = do
--     _ <- modify (\state -> state
--                 { on = not state.on })
--     pure next

