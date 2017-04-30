module Components.Router where
import Prelude
import Components.Chat as Chat
import Components.Login as Login
import Components.Navbar as Navbar
import Components.Sessions as Sessions
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.State.Class (modify)
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct (Coproduct)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen (Component)
import Halogen.Component.ChildPath (ChildPath, cp1, cp2, cp3)
import Halogen.HTML (HTML, a, div_, h1_, li_, slot', text, ul_)
import Routing (matchesAff)
import Routing.Match (Match)
import Routing.Match.Class (lit, num)

data Input a
  = Goto Routes a
  | IncomingMessage String a
  | HandleNavbar Navbar.Message a
  | HandleChat Chat.Message a

data CRUD
  = Index
  | Show Number

data Routes
  = Chat
  | Sessions CRUD
  | Home

data Message
  = OutputMessage String                 

init :: State
init = { currentPage: "Home" }

routing :: Match Routes
routing = chat
      <|> sessions
      <|> home

  where
    chat      = Chat <$ route "chat"
    home      = Home <$ lit ""
    sessions  = Sessions <$> (route "sessions" *> parseCRUD)
    route str = lit "" *> lit str
    parseCRUD = Show <$> num <|> pure Index

type State =
  { currentPage :: String
  }

type QueryP
  = Coproduct Input ChildQuery

type ChildQuery = Coproduct3 Sessions.Input Navbar.Input Chat.Input
type ChildSlot  = Either3    Sessions.Slot  Navbar.Slot  Chat.Slot

ui :: forall eff. Component HTML Input Unit Message (Login.LoginEff eff)
ui = H.parentComponent
  { initialState: const init
  , render
  , eval
  , receiver: const Nothing
  }
  where
  render :: State -> H.ParentHTML Input ChildQuery ChildSlot (Login.LoginEff eff)
  render st =
    div_
      [ slot' pathToNavbar Navbar.Slot Navbar.ui unit (HE.input HandleNavbar)
      , viewPage st.currentPage
      ]

  viewPage :: String -> H.ParentHTML Input ChildQuery ChildSlot (Login.LoginEff eff)
  viewPage "Sessions" = slot' pathToSessions Sessions.Slot Sessions.ui unit absurd
  viewPage "Chat"     = slot' pathToChat Chat.Slot Chat.ui "" (HE.input HandleChat)
  viewPage _          = div_ [ text "Du är hemma"]

  eval :: Input ~> H.ParentDSL State Input ChildQuery ChildSlot Message (Login.LoginEff eff)
  eval (Goto Chat next) = do
    modify (_ { currentPage = "Chat" })
    pure next
  eval (Goto (Sessions view) next) = do
    modify case view of
      Index  -> (_ { currentPage = "Sessions" })
      Show n -> (_ { currentPage = "Session " <> show n })
    pure next
  eval (Goto Home next) = do
    H.raise $ OutputMessage "We went somewhere!"
    modify (_ { currentPage = "Home" })
    pure next
  eval (IncomingMessage text next) = do
    let msgtext = "Received: " <> text
    _ <- H.query' pathToChat Chat.Slot (H.request (Chat.AddMessage msgtext))
    pure next
  eval (HandleNavbar (Navbar.SetPage page) next) = do
    pure next
  eval (HandleChat (Chat.OutputMessage msg) next) = do
    H.raise $ OutputMessage msg
    pure next

pathToSessions :: ChildPath Sessions.Input ChildQuery Sessions.Slot ChildSlot
pathToSessions = cp1

pathToNavbar :: ChildPath Navbar.Input ChildQuery Navbar.Slot ChildSlot
pathToNavbar = cp2

pathToChat :: ChildPath Chat.Input ChildQuery Chat.Slot ChildSlot
pathToChat = cp3

-- pathToLogin :: ChildPath Login.Input ChildQuery Login.Slot ChildSlot
-- pathToLogin = cp4

routeSignal :: forall eff. H.HalogenIO Input Message (Aff (HA.HalogenEffects eff))
            -> Aff (HA.HalogenEffects eff) Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new

redirects :: forall eff. H.HalogenIO Input Message (Aff (HA.HalogenEffects eff))
          -> Maybe Routes
          -> Routes
          -> Aff (HA.HalogenEffects eff) Unit
redirects driver _ =
  driver.query <<< H.action <<< Goto
