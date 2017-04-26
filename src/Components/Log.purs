module Components.Log where

import Prelude
import Data.Array as A
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Control.Monad.Aff (Aff)
import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Either (Either(Right))
import Data.Foreign (readString)
import Data.Foreign.Index (readProp)
import Data.Maybe (Maybe(Nothing, Just))
-- import Control.Alt ((<|>))
-- -- import Control.Monad.Aff.Class (liftAff)
-- import Control.Monad.Eff.Console (log)
-- import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax, defaultRequest)
-- import Network.HTTP.Affjax.Request (class Requestable, RequestContent, toRequest)
-- import Network.HTTP.Affjax.Response (ResponseType(..))

type State =
  { messages :: Array String
  , inputText :: String
  , username :: String
  , password :: String
  , token :: Maybe String
  }

data Query a
  = AddMessage String a
  | UpdateUsername String a
  | UpdatePassword String a
  | UpdateInputText String a
  | JoinRoom String a
  | SendLogin a
  | SendMessage a

data Message
  = OutputMessage String                 
  | OutputJoinRoom String
  | SetToken String                 


component :: forall eff. H.Component HH.HTML Query Unit Message (Aff (ajax :: AX.AJAX | eff))
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { messages: []
                 , username: "andersl"
                 , password: ""
                 , token: Nothing
                 , inputText: ""
                 }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.ol_ $ map (\msg -> HH.li_ [ HH.text msg ]) state.messages
      , HH.div_
        [ HH.text "Användare: "
        , HH.input
          [ HP.type_ HP.InputText
          , HP.value (state.username)
          , HE.onValueInput (HE.input UpdateUsername)
          ] 
        , HH.input
          [ HP.type_ HP.InputText
          , HP.value (state.password)
          , HE.onValueInput (HE.input UpdatePassword)
          ] 
        , HH.button
          [ HE.onClick (HE.input_ SendLogin) ]
          [ HH.text "Logga in " ]
          ]
      , HH.div_ []
      , HH.input
          [ HP.type_ HP.InputText
          , HP.value (state.inputText)
          , HE.onValueInput (HE.input UpdateInputText)
          ]
      , HH.button
          [ HE.onClick (HE.input_ SendMessage) ]
          [ HH.text "Skicka meddelande" ]
      , HH.button
          [ HE.onClick (HE.input_ (JoinRoom state.inputText)) ]
          [ HH.text "Join Room" ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Message (Aff (ajax :: AX.AJAX | eff))
  eval (AddMessage msg next) = do
    let incomingMessage = "Received: " <> msg
    H.modify \st -> st { messages = st.messages `A.snoc` incomingMessage }
    pure next
  eval (SendMessage next) = do
    st <- H.get
    let outgoingMessage = st.inputText
    H.raise $ OutputMessage outgoingMessage
    H.modify \st' -> st'
      { messages = st'.messages `A.snoc` ("Sending: " <> outgoingMessage)
      , inputText = "" }
    pure next              
  eval (UpdateUsername text next) = do
    H.modify (_ { username = text })
    pure next
  eval (UpdatePassword text next) = do
    H.modify (_ { password = text })
    pure next
  eval (UpdateInputText text next) = do
    H.modify (_ { inputText = text })
    pure next
  eval (JoinRoom room next) = do
    H.raise $ OutputJoinRoom room
    pure next
  eval (SendLogin next) = do
    username <- H.gets _.username
    password <- H.gets _.password
    let payload = encodeJson $ Cred { username: username, password: password }
    result <- H.liftAff $ AX.post fireUrl payload
    let x = case runExcept $ readProp "access_token" result.response of
              Right fs ->
                case runExcept $ readString fs of
                  Right s -> Just s
                  _ -> Nothing
              _ -> Nothing
    case x of
      Just msg -> do
        H.modify (_ { token = x })
        H.raise $ SetToken msg
      Nothing ->
        H.modify (_ { token = Nothing })
    pure next

-- let x = case runExcept (keys result.response ) of
--           Right txt -> joinWith " " txt
--           _    -> "---"

-- type MyResponse = { access_token :: String, expiration :: String }

-- gettoken :: forall r. { access_token :: String, expiration :: String | r } -> String
-- gettoken t = t.access_token

data Cred = Cred { username :: String, password :: String }

instance encodeJsonCred :: EncodeJson Cred where
  encodeJson (Cred cred)
    =  "username" := cred.username
    ~> "password" := cred.password
    ~> jsonEmptyObject

fireUrl :: String
fireUrl = "http://localhost:8081/api/login"

