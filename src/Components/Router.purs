module Components.Router where

import Prelude (type (~>), Unit, absurd, bind, const, discard, pure, show, unit, ($), (*>), (<$), (<$>), (<<<), (<>))
import Components.Chat as Chat
import Components.Login as Login
import Components.Navbar as Navbar
import Components.Sessions as Sessions
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.State.Class (modify)
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct (Coproduct)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen.Aff as HA
import Halogen.HTML.Events as HE
import Halogen.HTML (HTML, div_, slot', text)
import Halogen (Component, HalogenIO, action, parentComponent, query', raise, request)
import Halogen.Component (ParentHTML, ParentDSL)
import Halogen.Component.ChildPath (ChildPath, cp1, cp2, cp3)
import Routing (matchesAff)
import Routing.Match (Match)
import Routing.Match.Class (lit, num)

data Input a
  = Goto Routes a
  | IncomingMessage String a
  | HandleNavbar Navbar.Output a
  | HandleChat Chat.Output a

data CRUD
  = Index
  | Show Number

data Routes
  = Chat
  | Sessions CRUD
  | Home

data Output
  = OutputMessage String                 


routing :: Match Routes
routing = chat <|> sessions <|> home
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

ui :: forall eff. Component HTML Input Unit Output (Login.LoginEff eff)
ui = parentComponent
  { initialState: const { currentPage: "Home" }
  , render
  , eval
  , receiver: const Nothing
  }
  where
  render :: State -> ParentHTML Input ChildQuery ChildSlot (Login.LoginEff eff)
  render st =
    div_
      [ slot' pathToNavbar Navbar.Slot Navbar.ui unit (HE.input HandleNavbar)
      , viewPage st.currentPage
      ]

  viewPage :: String -> ParentHTML Input ChildQuery ChildSlot (Login.LoginEff eff)
  viewPage "Sessions" = slot' pathToSessions Sessions.Slot Sessions.ui unit absurd
  viewPage "Chat"     = slot' pathToChat Chat.Slot Chat.ui "" (HE.input HandleChat)
  viewPage _          = div_ [ text "Du är hemma"]

  eval :: Input ~> ParentDSL State Input ChildQuery ChildSlot Output (Login.LoginEff eff)
  eval (Goto Chat next) = do
    modify (_ { currentPage = "Chat" })
    pure next
  eval (Goto (Sessions view) next) = do
    modify case view of
      Index  -> (_ { currentPage = "Sessions" })
      Show n -> (_ { currentPage = "Session " <> show n })
    pure next
  eval (Goto Home next) = do
    raise $ OutputMessage "We went somewhere!"
    modify (_ { currentPage = "Home" })
    pure next
  eval (IncomingMessage text next) = do
    let msgtext = "Received: " <> text
    _ <- query' pathToChat Chat.Slot (request (Chat.AddMessage msgtext))
    pure next
  eval (HandleNavbar (Navbar.SetPage page) next) = do
    pure next
  eval (HandleNavbar (Navbar.GotToken token) next) = do
    raise $ OutputMessage token
    -- _ <- H.query' pathToChat Chat.Slot (H.request (Chat.AddMessage msgtext))
    pure next
  eval (HandleChat (Chat.OutputMessage msg) next) = do
    raise $ OutputMessage msg
    pure next

pathToSessions :: ChildPath Sessions.Input ChildQuery Sessions.Slot ChildSlot
pathToSessions = cp1

pathToNavbar :: ChildPath Navbar.Input ChildQuery Navbar.Slot ChildSlot
pathToNavbar = cp2

pathToChat :: ChildPath Chat.Input ChildQuery Chat.Slot ChildSlot
pathToChat = cp3

routeSignal :: forall eff. HalogenIO Input Output (Aff (HA.HalogenEffects eff))
            -> Aff (HA.HalogenEffects eff) Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new

redirects :: forall eff. HalogenIO Input Output (Aff (HA.HalogenEffects eff))
          -> Maybe Routes
          -> Routes
          -> Aff (HA.HalogenEffects eff) Unit
redirects driver _ =
  driver.query <<< action <<< Goto
