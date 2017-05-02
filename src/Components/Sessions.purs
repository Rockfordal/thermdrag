module Components.Sessions where

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Prelude (class Eq, class Ord, type (~>), Unit, Void, const, pure, unit)

derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot


type State = Unit

data Input a = Noop a

data Slot = Slot


ui :: forall e. H.Component HH.HTML Input Unit Void e
ui = H.component
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render _ =
      HH.div_
        [ HH.h1_ [ HH.text "Your Sessions" ]
        , HH.p_  [ HH.text "wow you lift a LOT" ]
        ]

    eval :: Input ~> H.ComponentDSL State Input Void e
    eval (Noop n) = pure n