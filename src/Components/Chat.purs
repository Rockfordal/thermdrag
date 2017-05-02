module Components.Chat where

-- import Components.Common (restUrl, getit)
import Control.Monad.Aff (Aff)
import Data.Array as A
import Halogen (Component, component, get, modify, raise)
import Halogen.Component (ComponentDSL)
import Halogen.HTML (HTML, br_, button, div_, input, li_, ol_, text)
import Halogen.HTML.Properties (InputType(InputText), class_, type_, value)
import Halogen.HTML.Events as HE
import Network.HTTP.Affjax (AJAX)
import Halogen.Themes.Bootstrap3
import Prelude (class Eq, class Ord, type (~>), const, discard, map, pure, ($), (<>), bind, show)

derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot


type AjaxEff e = Aff (ajax :: AJAX | e)

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

data Output
  = OutputMessage String


ui :: forall e. Component HTML Input String Output (AjaxEff e)
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

  eval :: Input ~> ComponentDSL State Input Output (AjaxEff e)
  eval (UpdateInputText text next) = do
    modify (_ { inputText = text })
    pure next

  eval (JoinRoom room next) = do
    raise $ OutputMessage $ """{"type":"join","roomname":""" <> "\"" <> room <> "\"" <> "}"
    modify (_ { room = room })
    pure next

  eval (SendMessage next) = do
    st <- get
    raise $ OutputMessage $
      showTextMsg { username: st.username
                  , payload:  st.inputText
                  , roomname: st.room
                  , typ: "msg" }
    let logtext = "Sending: " <> st.inputText
    modify \state -> state
       { messages  = state.messages `A.snoc` logtext
       , inputText = "" }
    pure next

  eval (AddMessage text reply) = do
    modify \state -> state
        { messages = state.messages `A.snoc` text }
    pure (reply "")

  eval (GetRooms next) = do
--     token <- H.gets _.token
--     result <- H.liftAff $ getit token roomsUrl
--     let _ = result.response :: String  -- required for compiler to understand type
--     case result.status of
--       StatusCode 200 -> do
--         H.modify (_ { token = Nothing })
--       StatusCode i -> do
--         H.modify (_ { inputText = show i })
    pure next

  -- roomsUrl = restUrl <> "/api/rooms"


showTextMsg :: TextMsg -> String
showTextMsg msg =
  "{" <> show "username" <> ":" <> show msg.username <>
  "," <> show "payload"  <> ":" <> show msg.payload <>
  "," <> show "roomname" <> ":" <> show msg.roomname <>
  "," <> show "type"     <> ":" <> show msg.typ <> "}"
