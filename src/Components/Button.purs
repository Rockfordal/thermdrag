module Components.Button where

import Prelude (type (~>), Unit, bind, not, ($), discard, pure, const)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Maybe (Maybe(..))
import Halogen (Component, component, get, put, raise)
import Halogen.Component (ComponentHTML, ComponentDSL)
import Halogen.HTML (HTML, button, text)

data Input a
  = Toggle a
  | IsOn (Boolean -> a)

type State = Boolean

data Output = Toggled Boolean

ui :: forall m. Component HTML Input Unit Output m
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

  eval :: Input ~> ComponentDSL State Input Output m
  eval = case _ of
    Toggle next -> do
      state <- get
      put (not state)
      raise $ Toggled (not state)
      pure next
    IsOn reply -> do
      state <- get
      pure $ reply state

