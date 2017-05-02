module Components.Navbar where

import Components.Login as Login
import Components.ViewHelpers (container_, navlink)
import Data.Maybe (Maybe(Nothing))
import Halogen.Themes.Bootstrap3 as B
import Halogen (Component, put, raise)
import Halogen.Component (ParentDSL, parentComponent)
import Halogen.HTML (HTML, a, nav, slot, text, ul)
import Halogen.HTML.Properties (classes, href)
import Halogen.HTML.Events as HE
import Prelude (class Eq, class Ord, type (~>), Unit, const, discard, map, pure, unit, ($))

derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

type State = Boolean

data Input a
  = UpdateInputText String a
  | HandleLogin Login.Output a

data Slot = Slot

data Output
  = SetPage  String
  | GotToken String


ui :: forall e. Component HTML Input Unit Output (Login.LoginEff e)
ui = parentComponent { initialState: const initial, render, eval, receiver: const Nothing }
  where
  initial = false

  render state =
    nav [ classes [ B.navbarNav, B.navbarFixedTop, B.navbarInverse] ]
      [ container_
        [ a [ classes [ B.navbarBrand ]
            , href "#/Home"
            ] [ text "SuperChat" ]
        , ul [ classes [ B.navbarNav, B.nav, B.navTabs] ]
          $ map navlink ["Sessions", "Chat"]
        , slot Login.Slot Login.ui unit $ HE.input HandleLogin
        ]
      ]

  eval :: Input ~> ParentDSL State Input Login.Input Login.Slot Output (Login.LoginEff e)
  eval (UpdateInputText text next) = do
    put true
    pure next

  eval (HandleLogin (Login.GotToken token) next) = do
    raise $ GotToken token
    pure next

