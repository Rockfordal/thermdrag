module Components.Login where

import Prelude (class Eq, class Ord, type (~>), Unit, bind, const, discard, pure, unit, ($), (<>))
import Control.Monad.Aff (Aff)
import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Either (Either(..))
import Data.Foreign (Foreign, readString)
import Data.Foreign.Index (readProp)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType.Common (applicationJSON)
import Data.Tuple (Tuple(..), fst, snd)
import Data.HTTP.Method (Method(..))
import Halogen (Component, component, gets, liftAff, modify, raise)
import Halogen.Component (ComponentHTML, ComponentDSL)
import Halogen.HTML (HTML, button, div, form, input, text)
import Halogen.HTML.Properties (ButtonType(..), InputType(..), class_, classes, placeholder, type_, value)
import Halogen.HTML.Events as HE
import Halogen.Themes.Bootstrap3 (btn, btnSuccess, formControl, formGroup, navbarForm, navbarRight)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax, defaultRequest, post)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.ResponseHeader (ResponseHeader)
import Network.HTTP.StatusCode (StatusCode(..))

derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

instance encodeJsonCred :: EncodeJson Cred where
  encodeJson (Cred cred)
    =  "username" := (fst cred)
    ~> "password" := (snd cred)
    ~> jsonEmptyObject


type LoginEff e = Aff (ajax :: AJAX | e)

type State =
  { username :: String
  , password :: String
  , token    :: Maybe String
  }

data Input a
  = UpdateUsername String a
  | UpdatePassword String a
  | SendLogin a
  | SendLogout a

data Cred = Cred (Tuple String String)

data Slot = Slot

data Output
  = GotToken String


ui :: forall e. Component HTML Input Unit Output (LoginEff e)
ui =
  component
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

  render :: State -> ComponentHTML Input
  render state =
    form [ classes [ navbarForm, navbarRight]]
      case state.token of
        Nothing ->
          [ div [ class_ formGroup ]
            [ text "Användare: "
            , input
              [ type_ InputText
              , placeholder "Användarnamn"
              , value state.username
              , class_ formControl
              , HE.onValueInput (HE.input UpdateUsername) ]
            , input
              [ type_ InputPassword
              , placeholder "Lösenord"
              , value state.password
              , class_ formControl
              , HE.onValueInput (HE.input UpdatePassword) ]
            , button
              [ type_ ButtonSubmit
              , classes [ btn, btnSuccess]
              , HE.onClick (HE.input_ SendLogin) ]
              [ text "Logga in " ]
            ]
          ]
        Just token ->
          [ button
            [ HE.onClick (HE.input_ SendLogout)
            , classes [ btn, btnSuccess]
            ] [ text "Logga ut " ]
          ]

  eval :: Input ~> ComponentDSL State Input Output (LoginEff e)
  eval (UpdateUsername text next) = do
    modify (_ { username = text })
    pure next
  eval (UpdatePassword text next) = do
    modify (_ { password = text })
    pure next
  eval (SendLogin next) = do
    username <- gets _.username
    password <- gets _.password
    let payload = encodeJson $ Cred (Tuple username password)
    result <- liftAff $ post loginUrl payload
    let x = mreadtoken result.response
    case x of
      Just msg -> do
        modify (_ { token = x })
        raise $ GotToken msg
      Nothing ->
        modify (_ { token = Nothing })
    pure next
  eval (SendLogout next) = do
    token <- gets _.token
    result <- liftAff $ getit token
    let _ = result.response :: String -- workaround for type
    case result.status of
      StatusCode 200 -> do
        modify (_ { token = Nothing })
      StatusCode _ -> do
        pure unit
    pure next


mreadtoken :: Foreign -> Maybe String
mreadtoken response =
  case runExcept $ readProp "access_token" response of
    Right fs ->
      case runExcept $ readString fs of
        Right s -> Just s
        _ -> Nothing
    _ -> Nothing

getit :: forall r e. Respondable r => Maybe String -> Aff (ajax :: AJAX | e)
         { status :: StatusCode, headers :: Array ResponseHeader, response :: r }
getit jwt = affjax $ rekvest jwt

rekvest :: Maybe String -> AffjaxRequest Unit
rekvest jwt = defaultRequest
  { url = logoutUrl
  , method = Left GET
  , headers = [ RequestHeader "Authorization" $ "Bearer " <> (fromMaybe "" jwt)
              , ContentType applicationJSON ] }

loginUrl :: String
loginUrl = baseUrl <> "/api/login"

logoutUrl :: String
logoutUrl = baseUrl <> "/api/logout"

baseUrl :: String
baseUrl = "http://localhost:8081"

