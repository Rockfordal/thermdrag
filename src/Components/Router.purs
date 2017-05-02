module Components.Router where

import Components.Chat as Chat
import Components.Navbar as Navbar
import Components.Sessions as Sessions
import Components.Login (LoginEff)
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.State.Class (modify)
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct (Coproduct)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(Nothing))
import Data.Tuple (Tuple(Tuple))
import Halogen (Component, HalogenIO, action, parentComponent, query', raise, request)
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.Component (ParentHTML, ParentDSL)
import Halogen.Component.ChildPath (ChildPath, cp1, cp2, cp3)
import Halogen.HTML (HTML, div_, slot', text)
import Halogen.HTML.Events as HE
import Prelude (type (~>), Unit, absurd, bind, const, discard, pure, show, unit, ($), (*>), (<$), (<$>), (<>), (>>>))
import Routing (matchesAff)
import Routing.Match (Match)
import Routing.Match.Class (lit, num)


type QueryP     = Coproduct  Input ChildQuery
type ChildQuery = Coproduct3 Sessions.Input Navbar.Input Chat.Input
type ChildSlot  = Either3    Sessions.Slot  Navbar.Slot  Chat.Slot

type State =
  { currentPage :: String
  }

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


ui :: forall e. Component HTML Input Unit Output (LoginEff e)
ui = parentComponent { initialState: const initial, render, eval, receiver: const Nothing }
  where
  initial = { currentPage: "Home" }

  render :: State -> ParentHTML Input ChildQuery ChildSlot (LoginEff e)
  render state =
    div_
      [ slot' pathToNavbar Navbar.Slot Navbar.ui unit $ HE.input HandleNavbar
      , viewPage state.currentPage
      ]

  viewPage :: String -> ParentHTML Input ChildQuery ChildSlot (LoginEff e)
  viewPage "Sessions" = slot' cp1    Sessions.Slot Sessions.ui unit absurd
  viewPage "Chat"     = slot' pathToChat Chat.Slot Chat.ui "" $ HE.input HandleChat
  viewPage _          = div_ [ text "Du är hemma" ]

  eval :: Input ~> ParentDSL State Input ChildQuery ChildSlot Output (LoginEff e)
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
    let logtext = "Received: " <> text
    _ <- query' pathToChat Chat.Slot $ request (Chat.AddMessage logtext)
    pure next

  eval (HandleNavbar (Navbar.SetPage page) next) = do
    pure next

  eval (HandleNavbar (Navbar.GotToken token) next) = do
    raise $ OutputMessage token
    pure next

  eval (HandleChat (Chat.OutputMessage text) next) = do
    raise $ OutputMessage text
    pure next


routing :: Match Routes
routing = chat <|> sessions <|> home
  where
    chat      = Chat <$ route "chat"
    home      = Home <$ lit ""
    sessions  = Sessions <$> (route "sessions" *> parseCRUD)
    route str = lit "" *> lit str
    parseCRUD = Show <$> num <|> pure Index

pathToSessions :: ChildPath Sessions.Input ChildQuery Sessions.Slot ChildSlot
pathToSessions = cp1

pathToNavbar :: ChildPath Navbar.Input ChildQuery Navbar.Slot ChildSlot
pathToNavbar = cp2

pathToChat :: ChildPath Chat.Input ChildQuery Chat.Slot ChildSlot
pathToChat = cp3

routeSignal :: forall e. HalogenIO Input Output
                         (Aff (HalogenEffects e))
                       -> Aff (HalogenEffects e) Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new

redirects :: forall e. HalogenIO Input Output (Aff (HalogenEffects e))
                  -> Maybe Routes -> Routes -> Aff (HalogenEffects e) Unit
redirects driver _ =
  Goto >>> action >>> driver.query
