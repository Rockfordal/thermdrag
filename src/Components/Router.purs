module Components.Router where

import Components.Chat as Chat
import Components.Draw as Draw
import Components.Navbar as Navbar
import Components.Sessions as Sessions
import Halogen.HTML.Events as HE
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.State.Class (modify)
import DOM (DOM)
import Data.Array (snoc)
import Data.Either.Nested (Either4)
import Data.Functor.Coproduct (Coproduct)
import Data.Functor.Coproduct.Nested (Coproduct4)
import Data.Maybe (Maybe(Nothing))
import Data.Tuple (Tuple(Tuple))
import Halogen (Component, HalogenIO, action, parentComponent, query', raise, request)
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.Component (ParentDSL)
import Halogen.Component.ChildPath (ChildPath, cp1, cp2, cp3, cp4)
import Halogen.HTML (HTML, div_, slot', text)
import Network.HTTP.Affjax (AJAX)
import Prelude (type (~>), Unit, absurd, bind, const, discard, pure, show, unit, ($), (*>), (<$), (<$>), (<>), (>>>))
import Routing (matchesAff)
import Routing.Match (Match)
import Routing.Match.Class (lit, num)

type RouterAff e = Aff (ajax :: AJAX, dom :: DOM | e)

type DSL e = ParentDSL State Input ChildQuery ChildSlot Output (RouterAff e)

type QueryP     = Coproduct  Input ChildQuery
type ChildQuery = Coproduct4 Navbar.Input Chat.Input Draw.Input Sessions.Input
type ChildSlot  = Either4    Navbar.Slot  Chat.Slot  Draw.Slot  Sessions.Slot

type State =
  { currentPage :: String
  , pages :: Array String
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
  | Draw
  | Sessions CRUD
  | Home

data Output
  = OutputMessage String


ui :: forall e. Component HTML Input Unit Output (RouterAff e)
ui = parentComponent { initialState: const initial, render, eval, receiver: const Nothing }
  where
  initial =
    { currentPage: "Home"
    , pages: ["Chat", "Draw"]
    }

  render state =
    div_
      [ slot' pathToNavbar Navbar.Slot Navbar.ui state.pages $ HE.input HandleNavbar
      , viewPage state.currentPage
      ]

  viewPage "Sessions" = slot' pathToSessions Sessions.Slot Sessions.ui Sessions.initial absurd
  viewPage "Draw"     = slot' pathToDraw     Draw.Slot     Draw.ui     unit absurd
  viewPage "Chat"     = slot' pathToChat     Chat.Slot     Chat.ui     "" $ HE.input HandleChat
  viewPage _          = div_ [ text "Du är hemma" ]


  eval :: Input ~> DSL e
  eval (Goto Home next) = modify (_ { currentPage = "Home" }) *> pure next
  eval (Goto Chat next) = modify (_ { currentPage = "Chat" }) *> pure next
  eval (Goto Draw next) = modify (_ { currentPage = "Draw" }) *> pure next
  eval (Goto (Sessions view) next) = do
    modify case view of
      Index  -> (_ { currentPage = "Sessions" })
      Show n -> (_ { currentPage = "Session " <> show n })
    pure next

  eval (IncomingMessage text next) = do
    let logtext = "Received: " <> text
    _ <- query' pathToChat Chat.Slot $ request (Chat.AddMessage logtext)
    pure next

  eval (HandleNavbar (Navbar.SetPage page) next) = pure next
  eval (HandleNavbar (Navbar.GotToken token) next) = do
    modify \state -> state
      { pages = state.pages `snoc` "Sessions" }
    raise $ OutputMessage token
    pure next

  eval (HandleChat (Chat.OutputMessage text) next) = do
    raise $ OutputMessage text
    pure next


routing :: Match Routes
routing = chat <|> draw <|> sessions <|> home
  where
    chat      = Chat <$ route "chat"
    draw      = Draw <$ route "draw"
    home      = Home <$ lit ""
    sessions  = Sessions <$> (route "sessions" *> parseCRUD)
    route str = lit "" *> lit str
    parseCRUD = Show <$> num <|> pure Index


pathToNavbar :: ChildPath Navbar.Input ChildQuery Navbar.Slot ChildSlot
pathToNavbar = cp1

pathToChat :: ChildPath Chat.Input ChildQuery Chat.Slot ChildSlot
pathToChat = cp2

pathToDraw :: ChildPath Draw.Input ChildQuery Draw.Slot ChildSlot
pathToDraw = cp3

pathToSessions :: ChildPath Sessions.Input ChildQuery Sessions.Slot ChildSlot
pathToSessions = cp4


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
