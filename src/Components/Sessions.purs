module Components.Sessions where

import Data.Maybe (Maybe(Nothing))
import Halogen (Component, component)
import Halogen.Component (ComponentDSL)
import Halogen.HTML (HTML, div_, h1_, p_, text)
import Prelude (class Eq, class Ord, type (~>), Unit, Void, const, pure, unit)

derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

type State = Unit

data Input a = Noop a

data Slot = Slot


ui :: forall e. Component HTML Input Unit Void e
ui = component { initialState: const initial, render, eval, receiver: const Nothing }
  where
    initial = unit

    render _ =
      div_
        [ h1_ [ text "Your Sessions" ]
        , p_  [ text "wow you lift a LOT" ]
        ]

    eval :: Input ~> ComponentDSL State Input Void e
    eval (Noop n) = pure n

