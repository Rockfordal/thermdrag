module Components.Navbar where

import Components.Login as Login
import Halogen.HTML.Events as HE
import Halogen.Themes.Bootstrap3 as B
import Components.ViewHelpers (container_, navlink)
import Halogen (Component, put, raise)
import Halogen.Component (ParentDSL, parentComponent)
import Halogen.HTML (HTML, a, nav, slot, text, ul)
import Halogen.HTML.Properties (classes, href)
import Prelude (class Eq, class Ord, type (~>), const, discard, map, pure, unit, ($))

derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

type State = Array String

data Input a
  = HandleLogin Login.Output a
  | SetPages (Array String) a

data Slot = Slot

data Output
  = SetPage  String
  | GotToken String


ui :: forall e. Component HTML Input State Output (Login.LoginEff e)
ui = parentComponent { initialState: const initial, render, eval, receiver: HE.input SetPages }
  where
  initial = ["No page found"]

  render state =
    nav [ classes [ B.navbarNav, B.navbarFixedTop, B.navbarInverse] ]
      [ container_
        [ a [ classes [ B.navbarBrand ]
            , href "#/Home"
            ] [ text "SuperChat" ]
        , ul [ classes [ B.navbarNav, B.nav, B.navTabs] ]
          $ map navlink state
        , slot Login.Slot Login.ui unit $ HE.input HandleLogin
        ]
      ]

  eval :: Input ~> ParentDSL State Input Login.Input Login.Slot Output (Login.LoginEff e)
  eval (HandleLogin (Login.GotToken token) next) = do
    raise $ GotToken token
    pure next

  eval (SetPages pages next) = do
    put pages
    pure next
