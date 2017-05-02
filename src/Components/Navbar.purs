module Components.Navbar where

import Components.Login as Login
import Halogen.HTML.Events as HE
import Halogen.Themes.Bootstrap3 as B
import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.String (toLower)
import Halogen (Component, put, raise)
import Halogen.Component (ParentDSL, ParentHTML, parentComponent)
import Halogen.HTML (HTML, a, div, li_, nav, slot, text, ul)
import Halogen.HTML.Properties (class_, classes, href)
import Prelude (class Eq, class Ord, type (~>), Unit, const, discard, map, pure, unit, ($), (<>))

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
ui =
  parentComponent
    { initialState: const false
    , render
    , eval
    , receiver: const Nothing
    }
  where
  render :: State -> ParentHTML Input Login.Input Login.Slot (Login.LoginEff e)
  render state =
    nav [ classes [ B.navbarNav, B.navbarFixedTop, B.navbarInverse] ]
      [ container_
        [ a [ classes [ B.navbarBrand ]
            , href "#/Home"
            ] [ text "SuperChat" ]
        , ul [ classes [ B.navbarNav, B.nav, B.navTabs] ]
          (map link ["Sessions", "Chat"])
        , slot Login.Slot Login.ui unit (HE.input HandleLogin)
        ]
      ]

  link s = li_ [ a [ href ("#/" <> toLower s) ] [ text s ] ]

  container attrs = div (class_ B.container : attrs)
  container_ = container []

  eval :: Input ~> ParentDSL State Input Login.Input Login.Slot Output (Login.LoginEff e)
  eval (UpdateInputText text next) = do
    put true
    pure next
  eval (HandleLogin (Login.GotToken token) next) = do
    raise $ GotToken token
    pure next

