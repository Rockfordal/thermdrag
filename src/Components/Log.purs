module Components.Log where

-- import Prelude
-- import Data.Array as A
-- import Halogen as H
-- import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
-- import Network.HTTP.Affjax as AX
-- import Control.Monad.Aff (Aff)
-- -- import Control.Monad.Except (runExcept)
-- -- import Data.Argonaut.Core (jsonEmptyObject)
-- -- import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
-- -- import Data.Either (Either(Right))
-- -- import Data.Foreign (readString)
-- -- import Data.Foreign.Index (readProp)
-- import Data.Maybe (Maybe(Nothing, Just))

-- type State =
--   { messages :: Array String
--   , inputText :: String
--   }

-- data Query a
--   = AddMessage String a
--   | UpdateInputText String a
--   | JoinRoom String a
--   | SendMessage a

-- data Message
--   = OutputMessage String                 
--   | OutputJoinRoom String


-- component :: forall eff. H.Component HH.HTML Query Unit Message (Aff (ajax :: AX.AJAX | eff))
-- component =
--   H.component
--     { initialState: const initialState
--     , render
--     , eval
--     , receiver: const Nothing
--     }
--   where
--   initialState :: State
--   initialState = { messages: [] , inputText: "" }

--   render :: State -> H.ComponentHTML Query
--   render state =
--     HH.div_
--       [ HH.ol_ $ map (\msg -> HH.li_ [ HH.text msg ]) state.messages
--       , HH.div_ []
--       , HH.input
--           [ HP.type_ HP.InputText
--           , HP.value (state.inputText)
--           , HE.onValueInput (HE.input UpdateInputText) ]
--       , HH.button
--           [ HE.onClick (HE.input_ SendMessage) ]
--           [ HH.text "Skicka meddelande" ]
--       , HH.button
--           [ HE.onClick (HE.input_ (JoinRoom state.inputText)) ]
--           [ HH.text "Join Room" ]
--       ]

--   eval :: Query ~> H.ComponentDSL State Query Message (Aff (ajax :: AX.AJAX | eff))
--   eval (AddMessage msg next) = do
--     let incomingMessage = "Received: " <> msg
--     H.modify \st -> st { messages = st.messages `A.snoc` incomingMessage }
--     pure next
--   eval (SendMessage next) = do
--     st <- H.get
--     let outgoingMessage = st.inputText
--     H.raise $ OutputMessage outgoingMessage
--     H.modify \st' -> st'
--       { messages = st'.messages `A.snoc` ("Sending: " <> outgoingMessage)
--       , inputText = "" }
--     pure next              
--   eval (UpdateInputText text next) = do
--     H.modify (_ { inputText = text })
--     pure next
--   eval (JoinRoom room next) = do
--     H.raise $ OutputJoinRoom room
--     pure next