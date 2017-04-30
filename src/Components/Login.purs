module Components.Login where

import Prelude
import Halogen as H
import Halogen.HTML.Events as HE
import Control.Monad.Aff (Aff)
import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Either (Either(..))
import Data.Foreign (readString)
import Data.Foreign.Index (readProp)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (applicationJSON)
import Halogen.HTML (HTML, div_, text, input, button)
import Halogen.HTML.Properties (InputType(..), type_, value)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax, defaultRequest, post)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.ResponseHeader (ResponseHeader)
import Network.HTTP.StatusCode (StatusCode(..))

type State =
  { username :: String
  , password :: String
  , token :: Maybe String
  }

data Input a
  = UpdateUsername String a
  | UpdatePassword String a
  | SendLogin a
  | SendLogout a

data Message
  = GotToken String                 

type LoginEff eff = Aff (ajax :: AJAX | eff)

data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

ui :: forall eff. H.Component HTML Input Unit Message (LoginEff eff)
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  initialState :: State
  initialState = { username: ""
                 , password: ""
                 , token: Nothing
                 }
  
  render :: State -> H.ComponentHTML Input
  render state =
    div_
      case state.token of
        Nothing ->
          [ text "Användare: "
          , input
            [ type_ InputText
            , value state.username
            , HE.onValueInput (HE.input UpdateUsername) ] 
          , input
            [ type_ InputPassword
            , value state.password
            , HE.onValueInput (HE.input UpdatePassword) ] 
          , button
            [ HE.onClick (HE.input_ SendLogin) ]
            [ text "Logga in " ]
          ] 
        Just token ->
          [ button
            [ HE.onClick (HE.input_ SendLogout) ]
            [ text "Logga ut " ]
          ]


  eval :: Input ~> H.ComponentDSL State Input Message (LoginEff eff)
  eval (UpdateUsername text next) = do
    H.modify (_ { username = text })
    pure next
  eval (UpdatePassword text next) = do
    H.modify (_ { password = text })
    pure next
  eval (SendLogin next) = do
    username <- H.gets _.username
    password <- H.gets _.password
    let payload = encodeJson $ Cred { username: username, password: password }
    result <- H.liftAff $ post loginUrl payload
    let x = case runExcept $ readProp "access_token" result.response of
              Right fs ->
                case runExcept $ readString fs of
                  Right s -> Just s
                  _ -> Nothing
              _ -> Nothing
    case x of
      Just msg -> do
        H.modify (_ { token = x })
        H.raise $ GotToken msg
      Nothing ->
        H.modify (_ { token = Nothing })
    pure next
  eval (SendLogout next) = do
    token <- H.gets _.token
    result <- H.liftAff $ getit token
    let _ = result.response :: String
    case result.status of
      StatusCode 200 -> do
        H.modify (_ { token = Nothing })
      StatusCode i -> do
        H.modify (_ { password = show i })
    pure next


getit :: forall t227 t229. Respondable t227 => Maybe String -> Aff ( ajax :: AJAX | t229)
                            { status :: StatusCode
                            , headers :: Array ResponseHeader
                            , response :: t227
                            }
getit jwt = affjax $ rekvest jwt


rekvest :: Maybe String -> AffjaxRequest Unit
rekvest jwt = defaultRequest
            { url = logoutUrl
            , method = Left GET
            , headers = [ RequestHeader "Authorization" $ "Bearer " <> (fromMaybe "" jwt) 
                        , Accept $ MediaType "*/*"  
                        , ContentType applicationJSON ] }


data Cred = Cred { username :: String, password :: String }

instance encodeJsonCred :: EncodeJson Cred where
  encodeJson (Cred cred)
    =  "username" := cred.username
    ~> "password" := cred.password
    ~> jsonEmptyObject

loginUrl :: String
loginUrl = baseUrl <> "/api/login"

logoutUrl :: String
logoutUrl = baseUrl <> "/api/logout"

baseUrl :: String
baseUrl = "http://localhost:8081"

