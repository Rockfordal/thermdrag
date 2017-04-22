module Components.Button where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Components.Init (initialState)
import Components.Typer (State, Query(..), Message(..))
import Components.View (render)
import Data.Maybe (Maybe(..))
import Control.Monad.Eff (Eff)

import Halogen.Aff as HA
import Components.Button (myButton)
import Halogen.VDom.Driver (runUI)

myButton :: forall m. H.Component HH.HTML Query Unit Message m
myButton =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Toggle next -> do
      state <- H.get
      let nextState = not state
      H.put nextState
      H.raise $ Toggled nextState
      pure next
    IsOn reply -> do
      state <- H.get
      pure (reply state)


buttonapp :: Eff (HA.HalogenEffects ()) Unit
buttonapp = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI myButton unit body