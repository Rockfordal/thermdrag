module Components.Login where

import Halogen.HTML.Events as HE
import Components.Common (getit, parsetoken, postit)
import Components.Config (restUrl)
import Control.Monad.Aff (Aff)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(Tuple), fst, snd)
import Halogen (Component, component, gets, liftAff, modify, raise)
import Halogen.Component (ComponentHTML, ComponentDSL)
import Halogen.HTML (HTML, button, div, form, input, text)
import Halogen.HTML.Properties (ButtonType(ButtonSubmit), InputType(InputText, InputPassword), class_, classes, placeholder, type_, value)
import Halogen.Themes.Bootstrap3 (btn, btnSuccess, formControl, formGroup, navbarForm, navbarRight)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.StatusCode (StatusCode(StatusCode))
import Prelude (class Eq, class Ord, type (~>), Unit, bind, const, discard, pure, unit, ($), (<>))

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
ui = component { initialState: const initial, render, eval, receiver: const Nothing }
  where
  initial = { username: ""
            , password: ""
            , token: Nothing
            }

  render :: State -> ComponentHTML Input
  render state =
    form [ classes [ navbarForm, navbarRight ]]
      case state.token of
        Nothing ->
          [ div [ class_ formGroup ]
            [ text "Användare: "
            , input
              [ type_ InputText
              , placeholder "Användarnamn"
              , value state.username
              , class_ formControl
              , HE.onValueInput $ HE.input UpdateUsername ]
            , input
              [ type_ InputPassword
              , placeholder "Lösenord"
              , value state.password
              , class_ formControl
              , HE.onValueInput $ HE.input UpdatePassword ]
            , button
              [ type_ ButtonSubmit
              , classes [ btn, btnSuccess]
              , HE.onClick $ HE.input_ SendLogin ]
              [ text "Logga in " ]
            ]
          ]
        Just token ->
          [ button
            [ HE.onClick $ HE.input_ SendLogout
            , classes [ btn, btnSuccess]
            ] [ text "Logga ut " ] ]


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
    result <- postit loginUrl payload
    let resp = parsetoken result.response
    case resp of
      Just token -> do
        modify (_ { token = resp })
        raise $ GotToken token
      Nothing ->
        modify (_ { token = Nothing })
    pure next

  eval (SendLogout next) = do
    token <- gets _.token
    result <- liftAff $ getit token logoutUrl
    let _ = result.response :: String -- workaround for type
    case result.status of
      StatusCode 200 -> do
        modify (_ { token = Nothing })
      StatusCode _ ->
        pure unit
    pure next


logoutUrl :: String
logoutUrl = restUrl <> "/api/logout"

loginUrl :: String
loginUrl = restUrl <> "/api/login"