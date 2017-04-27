module Components.Base where

import Prelude
import Components.Login as Login
import Data.Array as A
import Halogen as H
import Halogen.HTML.Events as HE
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(Nothing))
import Halogen.HTML (HTML, div_, li_, ol_, slot, text)
import Network.HTTP.Affjax (AJAX)

type State =
  { messages :: Array String
  }

data Query a
  = AddMessage String a
  | HandleLogin Login.Message a
--   | SendMessage a

data Message
  = OutputMessage String                 
  -- | OutputJoinRoom String

data Slot = LoginSlot
derive instance eqLoginSlot  :: Eq Slot
derive instance ordLoginSlot :: Ord Slot

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
  initialState = { messages: [] }

  render :: State -> H.ParentHTML Query Login.Query Slot (BaseEff eff)
  render state =
    div_
      [ ol_ $ map (\msg -> li_ [ text msg ]) state.messages
      , slot LoginSlot Login.component unit (HE.input HandleLogin)
      ]

  eval :: Query ~> H.ParentDSL State Query Login.Query Slot Message (BaseEff eff)
  eval (AddMessage msg next) = do
    let incomingMessage = "Received: " <> msg
    H.modify \st -> st { messages = st.messages `A.snoc` incomingMessage }
    pure next
  eval (HandleLogin (Login.GotToken tokstr) next) = do
    -- H.modify (\st -> st)
    H.raise $ OutputMessage tokstr
    pure next

--   eval (SendMessage next) = do
--     st <- H.get
--     let outgoingMessage = st.inputText
--     H.raise $ OutputMessage outgoingMessage
--     H.modify \st' -> st'
--       { messages = st'.messages `A.snoc` ("Sending: " <> outgoingMessage)
--       , inputText = "" }
--     pure next              

--   eval (JoinRoom room next) = do
--     H.raise $ OutputJoinRoom room
--     pure next

