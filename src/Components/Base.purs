module Components.Base where

import Prelude
import Components.Chat as Chat
import Components.Login as Login
import Halogen as H
import Halogen.HTML.Events as HE
import Control.Monad.Aff (Aff)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(Nothing))
import Halogen.Component.ChildPath (cp1, cp2)
import Halogen.HTML (HTML, div_, slot')
import Network.HTTP.Affjax (AJAX)

type State =
  { dummy :: Int }

data Query a
  = IncomingMessage String a
  | HandleLogin Login.Message a
  | HandleChat Chat.Message a

data Message
  = OutputMessage String                 

type ChildQuery = Coproduct2 Login.Query Chat.Query
type ChildSlot = Either2 Unit Unit 

type BaseEff eff = Aff (ajax :: AJAX | eff)

component :: forall eff. H.Component HTML Query Unit Message (BaseEff eff)
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { dummy: 0 }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (BaseEff eff)
  render state =
    div_
      [ slot' cp1 unit Login.component unit (HE.input HandleLogin)
      , slot' cp2 unit Chat.component  ""   (HE.input HandleChat)
      ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (BaseEff eff)
  eval (IncomingMessage text next) = do
    let msgtext = "Received: " <> text
    s <- H.query' cp2 unit (H.request (Chat.AddMessage msgtext))
    pure next
  eval (HandleLogin (Login.GotToken tokstr) next) = do
    H.raise $ OutputMessage tokstr
    pure next
  eval (HandleChat (Chat.OutputMessage msg) next) = do
    H.raise $ OutputMessage msg
    pure next
