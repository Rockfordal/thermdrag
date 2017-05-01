module Components.Chat where

import Data.Array as A
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as B
import Control.Monad.Aff (Aff)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe, fromMaybe)
import Data.MediaType.Common (applicationJSON)
import Halogen (Component, component, get, modify, raise)
import Halogen.Component (ComponentHTML, ComponentDSL)
import Halogen.HTML (HTML, br_, button, div_, input, li_, ol_, text)
import Halogen.HTML.Properties (class_, type_, value)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax, defaultRequest)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.ResponseHeader (ResponseHeader)
import Network.HTTP.StatusCode (StatusCode)
import Prelude (class Eq, class Ord, type (~>), Unit, const, discard, map, pure, ($), (<>), bind, show)

type State =
  { messages :: Array String
  , inputText :: String
  , room :: String
  , username :: String
  -- , rooms :: List String
  }

data Input a
  = UpdateInputText String a
  | JoinRoom String a
  | AddMessage String (String -> a)
  | SendMessage a
  | GetRooms a

data Message
  = OutputMessage String                 

data Slot = Slot

derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

type TextMsg =
  { username :: String
  , roomname :: String
  , payload  :: String
  , typ      :: String
  }

ui :: forall eff. Component HTML Input String Message (Aff (ajax :: AJAX | eff))
ui =
  component
    { initialState: const initialState
    , render
    , eval
    , receiver: HE.input UpdateInputText
    }
  where
  initialState :: State
  initialState = { messages: [], inputText: "", room: "", username: "andersl" }

  render :: State -> ComponentHTML Input
  render state =
    div_
      [ ol_ $ map (\msg -> li_ [ text msg ]) state.messages
      , div_ []
      , button
          [ HE.onClick (HE.input_ SendMessage)
          , class_ B.btn ]
          [ text "Skicka meddelande" ]
      , button
          [ HE.onClick (HE.input_ (JoinRoom state.inputText))
          , class_ B.btn ]
          [ text "Join Room" ]
      , button
          [ HE.onClick (HE.input_ GetRooms)
          , class_ B.btn ]
          [ text "Get Rooms" ]
      , br_
      , br_
      , input
          [ type_ HP.InputText
          , class_ B.formControl
          , value (state.inputText)
          , HE.onValueInput (HE.input UpdateInputText) ]
      ]

  eval :: Input ~> ComponentDSL State Input Message (Aff (ajax :: AJAX | eff))
  eval (UpdateInputText text next) = do
    modify (_ { inputText = text })
    pure next
  eval (JoinRoom room next) = do
    raise $ OutputMessage $ """{"type":"join","roomname":""" <> "\"" <> room <> "\"" <> "}"
    modify (_ { room = room })
    pure next
  eval (SendMessage next) = do
    state <- get
    let logmsg = "Sending: " <> state.inputText
    let out = showTextMsg { username: state.username, payload: state.inputText, roomname: state.room, typ: "msg"}
    raise $ OutputMessage out
    modify \st -> st
      { messages = st.messages `A.snoc` logmsg
      , inputText = "" }
    pure next              
  eval (AddMessage text reply) = do
    modify \st -> st { messages = st.messages `A.snoc` text }
    pure (reply "")
  eval (GetRooms next) = do
--     token <- H.gets _.token
--     result <- H.liftAff $ getit token
--     let _ = result.response :: String
--     case result.status of
--       StatusCode 200 -> do
--         H.modify (_ { token = Nothing })
--       StatusCode i -> do
--         H.modify (_ { inputText = show i })
    pure next


getit :: forall r eff. Respondable r => Maybe String -> Aff ( ajax :: AJAX | eff)
              { status :: StatusCode , headers :: Array ResponseHeader , response :: r }
getit jwt = affjax $ rekvest jwt


rekvest :: Maybe String -> AffjaxRequest Unit
rekvest jwt = defaultRequest
            { url = roomsUrl
            , method = Left GET
            , headers = [ RequestHeader "Authorization" $ "Bearer " <> (fromMaybe "" jwt) 
                        , ContentType applicationJSON ] }

roomsUrl :: String
roomsUrl = baseUrl <> "/api/rooms"

baseUrl :: String
baseUrl = "http://localhost:8081"

showTextMsg :: TextMsg -> String
showTextMsg msg = "{" <> show "username" <> ":" <> show msg.username <>
                  "," <> show "payload"  <> ":" <> show msg.payload <>
                  "," <> show "roomname" <> ":" <> show msg.roomname <>
                  "," <> show "type"     <> ":" <> show msg.typ <> "}"
