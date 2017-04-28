module Components.Chat where

import Prelude
import Data.Array as A
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe, fromMaybe)
import Data.MediaType.Common (applicationJSON)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax, defaultRequest)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.ResponseHeader (ResponseHeader)
import Network.HTTP.StatusCode (StatusCode())
-- import Data.Argonaut.Core (jsonEmptyObject)
-- import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))

type State =
  { messages :: Array String
  , inputText :: String
  , room :: String
  , username :: String
  -- , rooms :: List String
  }

data Query a
  = UpdateInputText String a
  | JoinRoom String a
  | AddMessage String (String -> a)
  | SendMessage a
  | GetRooms a

data Message
  = OutputMessage String                 

component :: forall eff. H.Component HH.HTML Query String Message (Aff (ajax :: AJAX | eff))
-- component :: forall eff. H.Component HH.HTML Query String Message eff
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: HE.input UpdateInputText
    }
  where
  initialState :: State
  initialState = { messages: [] , inputText: "", room: "", username: "andersl" }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.ol_ $ map (\msg -> HH.li_ [ HH.text msg ]) state.messages
      , HH.div_ []
      , HH.input
          [ HP.type_ HP.InputText
          , HP.value (state.inputText)
          , HE.onValueInput (HE.input UpdateInputText) ]
      , HH.button
          [ HE.onClick (HE.input_ SendMessage) ]
          [ HH.text "Skicka meddelande" ]
      , HH.button
          [ HE.onClick (HE.input_ (JoinRoom state.inputText)) ]
          [ HH.text "Join Room" ]
      , HH.button
          [ HE.onClick (HE.input_ GetRooms) ]
          [ HH.text "Get Rooms" ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Message (Aff (ajax :: AJAX | eff))
--   eval :: Query ~> H.ComponentDSL State Query Message eff
  eval (UpdateInputText text next) = do
    H.modify (_ { inputText = text })
    pure next
  eval (JoinRoom room next) = do
    H.raise $ OutputMessage $ """{"type":"join","roomname":""" <> "\"" <> room <> "\"" <> "}"
    H.modify (_ { room = room })
    pure next
  eval (SendMessage next) = do
    state <- H.get
    let logmsg = "Sending: " <> state.inputText
    let out = showTextMsg { username: state.username, payload: state.inputText, roomname: state.room, typ: "msg"}
    H.raise $ OutputMessage out
    H.modify \st -> st
      { messages = st.messages `A.snoc` logmsg
      , inputText = "" }
    pure next              
  eval (AddMessage text reply) = do
    H.modify \st -> st { messages = st.messages `A.snoc` text }
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


getit :: forall t227 t229. Respondable t227 => Maybe String -> Aff ( ajax :: AJAX | t229)
                            { status :: StatusCode
                            , headers :: Array ResponseHeader
                            , response :: t227
                            }
getit jwt = affjax $ rekvest jwt


rekvest :: Maybe String -> AffjaxRequest Unit
rekvest jwt = defaultRequest
            { url = roomsUrl
            , method = Left GET
            , headers = [ RequestHeader "Authorization" $ "Bearer " <> (fromMaybe "" jwt) 
                        -- , Accept $ MediaType "*/*"  
                        , ContentType applicationJSON ] }

roomsUrl :: String
roomsUrl = baseUrl <> "/api/rooms"

baseUrl :: String
baseUrl = "http://localhost:8081"


type TextMsg =
  { username :: String
  , roomname :: String
  , payload :: String
  , typ  :: String
  }

showTextMsg :: TextMsg -> String
showTextMsg msg = "{" <> show "username" <> ":" <> show msg.username <>
                  "," <> show "payload"  <> ":" <> show msg.payload <>
                  "," <> show "roomname" <> ":" <> show msg.roomname <>
                  "," <> show "type"     <> ":" <> show msg.typ <> "}"

-- data TextMsg = TextMsg { username :: String, payload :: String, roomname :: String, typ :: String } 

-- "andersl","payload":"tjena","roomname":"Arduino","type":"msg"}

-- data TextMsg = TextMsg String String String String
-- instance showTM :: Show TextMsg where
--   show _ _ _ _ = "saknas"

-- instance encodeJsonTextMsg :: EncodeJson TextMsg where
--   encodeJson (TextMsg msg)
--     =  "username" := msg.username
--     ~> "payload" := msg.payload
--     ~> "roomname" := msg.roomname
--     ~> "type" := msg.type
--     ~> jsonEmptyObject

