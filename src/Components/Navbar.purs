module Components.Navbar where

import Halogen.Themes.Bootstrap3 as B
import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.String (toLower)
import Halogen (Component, component, modify)
import Halogen.Component (ComponentHTML, ComponentDSL)
import Halogen.HTML (HTML, a, div, li_, nav, text, ul)
import Halogen.HTML.Properties (class_, classes, href)
import Prelude hiding (div)

type State =
  { dummy :: Boolean }

data Input a
  = UpdateInputText String a

data Message
  = SetPage String                 

data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

ui :: forall eff. Component HTML Input Unit Message eff
ui =
  component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  initialState :: State
  initialState = { dummy: false }

  render :: State -> ComponentHTML Input
  render state =
    nav [ classes [ B.navbarNav, B.navbarFixedTop, B.navbarInverse] ]
      [ container_
        [ a [ classes [ B.navbarBrand ]
            , href "#/Home"
            ] [ text "SuperChat" ]
        , ul [ classes [ B.navbarNav, B.nav, B.navTabs] ]
          (map link ["Sessions", "Chat"])
        -- , case muser of
        --        Nothing ->
        --            H.ul [ P.classes [ B.nav, B.navbarNav, B.navTabs, B.navbarRight ] ]
        --                [ H.li_ [ linkTo Login "Log in" ]
        --                , H.li_ [ linkTo Registration "Sign up" ] ]
        --        Just u ->
        --            H.ul [ P.classes [ B.nav, B.navbarNav, B.navTabs, B.navbarRight ] ]
        --                [ H.li_ [ linkTo Logout "Log out" ] ]
        ]
      ]

  link s = li_ [ a [ href ("#/" <> toLower s) ] [ text s ] ]

  container attrs = div (class_ B.container : attrs)
  container_ = container []

  eval :: Input ~> ComponentDSL State Input Message eff
  eval (UpdateInputText text next) = do
    modify (_ { dummy = true })
    pure next

