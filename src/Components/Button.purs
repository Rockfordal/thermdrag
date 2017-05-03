module Components.Button where

import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Maybe (Maybe(Nothing))
import Halogen (Component, component, get, put, raise)
import Halogen.Component (ComponentDSL)
import Halogen.HTML (HTML, button, text)
import Halogen.HTML.Properties (class_)
import Halogen.Themes.Bootstrap3 (btn)
import Prelude (class Eq, class Ord, type (~>), Unit, bind, const, discard, not, pure, ($))

derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

type DSL e = ComponentDSL State Input Output e

type State = Boolean

data Input a
  = Toggle a
  | IsOn (Boolean -> a)

data Slot = Slot

data Output = NewState Boolean


ui :: forall e. Component HTML Input Unit Output e -- (Reader GlobalState IO)
ui = component { initialState: const initial, render, eval, receiver: const Nothing }
  where
  initial = false

  render state =
    let label = if state then "On" else "Off"
    in button
      [ HP.title label
      , HE.onClick (HE.input_ Toggle)
      , class_ btn
      ] [ text label ]

  eval :: Input ~> DSL e
  eval = case _ of
    Toggle next -> do
      state <- get
      let newstate = not state
      put newstate
      raise $ NewState newstate
      pure next

    IsOn reply -> do
      state <- get
      pure $ reply state

