module Components.Log where

import Prelude
import Data.Array as A
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Control.Monad.Aff (Aff)
import DOM.HTML.HTMLElement (offsetHeight)
import Data.Maybe (Maybe(..))
import Halogen (liftEff)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax.Request (toRequest)
import Network.HTTP.Affjax.Response (ResponseType(..))

type State =
  { messages :: Array String
  , inputText :: String
  , username :: String
  , password :: String
  }

data Query a
  = AddMessage String a
  | UpdateUsername String a
  | UpdatePassword String a
  | UpdateInputText String a
  | SendLogin a
  | SendMessage a

data Message
  = OutputMessage String                 


--ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX | eff))
--component :: forall m. H.Component HH.HTML Query Unit Message m
component :: forall eff. H.Component HH.HTML Query Unit Message (Aff (ajax :: AX.AJAX))
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
      ]

  --eval :: Query ~> H.ComponentDSL State Query Message m
  --eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AX.AJAX | eff))
  eval :: Query ~> H.ComponentDSL State Query Message (Aff (ajax :: AX.AJAX))
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
  eval (SendLogin next) = do
    --username <- H.gets _.username
    --result <- getsome
    --let response = result.response
    --H.raise $ OutputMessage "sent"
    --H.modify (_ { password = "" })
    pure next
  eval (UpdateUsername text next) = do
    H.modify (_ { username = text })
    pure next
  eval (UpdatePassword text next) = do
    H.modify (_ { username = text })
    pure next
  eval (UpdateInputText text next) = do
    H.modify (_ { inputText = text })
    pure next

{-
   getsome :: forall t2 t4 t5.
      MonadAff
      ( ajax :: AJAX
      | t5
      )
      t2
      => Respondable t4 => t2
                            { status :: StatusCode
                            , headers :: Array ResponseHeader
                            , response :: t4
                            }
-}
                            
getsome = H.liftAff (AX.get fireUrl)

fireUrl :: String
fireUrl = "http://fire:4000/api/login"

payload :: String
payload = "123"
