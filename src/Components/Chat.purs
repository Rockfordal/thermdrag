module Components.Chat where

-- import Components.Common (restUrl, getit)
import Halogen.Themes.Bootstrap3
import Halogen.HTML.Events as HE
import Components.Common (getit)
import Components.Config (restUrl)
import Control.Monad.Aff (Aff)
import Data.Array (snoc)
import Data.Maybe (Maybe(..))
import Halogen (Component, component, get, liftAff, modify, raise)
import Halogen.Component (ComponentDSL)
import Halogen.HTML (HTML, br_, button, div_, input, li_, ol_, text)
import Halogen.HTML.Properties (InputType(InputText), class_, type_, value)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.StatusCode (StatusCode(..))
import Prelude (class Eq, class Ord, type (~>), bind, const, discard, map, pure, show, ($), (<>))

derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

type AjaxEff e = Aff (ajax :: AJAX | e)

type DSL e = ComponentDSL State Input Output (AjaxEff e)

type TextMsg =
  { username :: String
  , roomname :: String
  , payload  :: String
  , typ      :: String
  }

type State =
  { messages  :: Array String
  , inputText :: String
  , room      :: String
  , username  :: String
  }

data Input a
  = UpdateInputText String a
  | JoinRoom String a
  | AddMessage String (String -> a)
  | SendMessage a
  | GetRooms a

data Slot = Slot

data Output = OutputMessage String


ui :: ∀ e. Component HTML Input String Output (AjaxEff e)
ui = component { initialState: const initial, render, eval, receiver: HE.input UpdateInputText }
  where
  initial = { messages: []
            , inputText: ""
            , username: "andersl"
            , room: ""
            }

  render state =
    div_
      [ ol_ $ map viewitem state.messages
      , div_ []
      , button
          [ HE.onClick $ HE.input_ SendMessage
          , class_ btn ]
          [ text "Send meddelande" ]
      , button
          [ HE.onClick $ HE.input_ (JoinRoom state.inputText)
          , class_ btn ]
          [ text "Join Room" ]
      , button
          [ HE.onClick $ HE.input_ GetRooms
          , class_ btn ]
          [ text "Get Rooms" ]
      , br_
      , br_
      , input
          [ type_ InputText
          , class_ formControl
          , value state.inputText
          , HE.onValueInput $ HE.input UpdateInputText ]
      ]

  viewitem m = li_ [ text m ]

  eval :: Input ~> DSL e
  eval (UpdateInputText text next) = do
    modify (_ { inputText = text })
    pure next

  eval (JoinRoom room next) = do
    raise $ OutputMessage $ showJoinRoom room
    modify (_ { room = room })
    pure next

  eval (SendMessage next) = do
    st <- get
    raise $ OutputMessage $
      showTextMsg { username: st.username
                  , payload:  st.inputText
                  , roomname: st.room
                  , typ: "msg"
                  }
    let logtext = "Sending: " <> st.inputText
    modify \state -> state
      { messages  = state.messages `snoc` logtext
      , inputText = "" }
    pure next

  eval (AddMessage text reply) = do
    modify \state -> state
      { messages = state.messages `snoc` text }
    pure (reply "")

  eval (GetRooms next) = do
    -- token <- get _.token
    let token = Nothing
    result <- liftAff $ getit token roomsUrl
    let _ = result.response :: String  -- required for compiler to understand type
    case result.status of
      StatusCode 200 -> do
        modify (_ { inputText = "ok" })
      StatusCode i -> do
        modify (_ { inputText = show i })
    pure next

  roomsUrl = restUrl <> "/api/rooms"


showTextMsg :: TextMsg -> String
showTextMsg m =
  "{" <> show "username" <> ":" <> show m.username <>
  "," <> show "payload"  <> ":" <> show m.payload <>
  "," <> show "roomname" <> ":" <> show m.roomname <>
  "," <> show "type"     <> ":" <> show m.typ <> "}"


showJoinRoom :: String -> String
showJoinRoom room =
  """{"type":"join","roomname":""" <> "\"" <> room <> "\"" <> "}"

