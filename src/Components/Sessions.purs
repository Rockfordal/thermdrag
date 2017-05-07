module Components.Sessions where

import Components.Button as Button
import Halogen.HTML.Events as HE
import Components.Common (getSessionDb)
import Control.Monad.Aff (Aff)
import DOM (DOM)
import DOM.WebStorage.Storage (getItem, length, removeItem, setItem)
import Data.Maybe (Maybe(..))
import Halogen (Component, gets, liftEff, modify, parentComponent)
import Halogen.Component (ParentDSL)
import Halogen.HTML (HTML, button, div_, h1_, p_, slot, text)
import Halogen.HTML.Properties (classes)
import Halogen.Themes.Bootstrap3 (btn, btnDanger, btnSuccess)
import Prelude (class Eq, class Ord, type (~>), Void, bind, const, discard, pure, show, unit, ($), (<>), (>>=))

derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

type DomAff e = Aff ( dom :: DOM | e)

type DSL e = ParentDSL State Input Button.Input Button.Slot Void (DomAff e)

type State =
  { knapp :: Boolean
  , antal :: Int
  , saved :: String
  }

data Input a
  = HandleButton Button.Output a
  | Save a
  | Load a
  | Delete a

data Slot = Slot


initial :: State
initial = { knapp: false, antal: 0, saved: "0" }

ui :: ∀ e. Component HTML Input State Void (DomAff e)
ui = parentComponent { initialState: const initial, render, eval, receiver: const Nothing }
  where
  render state =
    div_
      [ h1_ [ text "Your Sessions" ]
      , p_  [ text $ "A toggle button: (it is " <> show state.knapp <> ")" ]
      , slot Button.Slot Button.ui unit $ HE.input HandleButton
      , p_ [ text $ "Lagrat värde: " <> state.saved ]
      , p_ [ text $ "Dbstorlek: " <> show state.antal ]
      , button
        [ classes [ btn, btnSuccess ]
        , HE.onClick $ HE.input_ Save
        ] [ text "Save" ]
      , button
        [ classes [ btn, btnSuccess ]
        , HE.onClick $ HE.input_ Load
        ] [ text "Load" ]
      , button
        [ classes [ btn, btnDanger ]
        , HE.onClick $ HE.input_ Delete
        ] [ text "Delete" ]
      ]

  eval :: Input ~> DSL e
  eval (Save next) = do
    antal <- gets (\s -> s.antal)
    liftEff $ getSessionDb >>= (setItem "saved" $ show antal)
    pure next

  eval (Load next) = do
    antal <- liftEff $ getSessionDb >>= (getItem "saved")
    -- case antal >>= fromString of
    case antal of
      Just s ->
        modify (_ { saved = s })
      Nothing ->
        pure unit
    pure next

  eval (Delete next) = do
    liftEff $ getSessionDb >>= (removeItem "saved")
    pure next

  eval (HandleButton (Button.NewState b) next) = do
    len <- liftEff $ getSessionDb >>= length
    modify (_ { antal = len
              , knapp = b })
    pure next

