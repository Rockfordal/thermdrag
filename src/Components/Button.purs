module Components.Button where

import Prelude (type (~>), Unit, bind, not, ($), discard, pure, const)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Maybe (Maybe(..))
import Halogen (Component, component, get, put, raise)
import Halogen.Component (ComponentHTML, ComponentDSL)
import Halogen.HTML (HTML, button, text)


type State = Boolean

data Input a
  = Toggle a
  | IsOn (Boolean -> a)

data Output = NewState Boolean


ui :: forall e. Component HTML Input Unit Output e
ui =
  component
    { initialState: const false
    , render
    , eval
    , receiver: const Nothing
    }
  where
  render :: State -> ComponentHTML Input
  render state =
    let label = if state then "On" else "Off"
    in button
      [ HP.title label
      , HE.onClick (HE.input_ Toggle)
      ] [ text label ]

  eval :: Input ~> ComponentDSL State Input Output e
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

