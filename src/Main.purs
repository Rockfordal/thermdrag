module Main (main) where

import Prelude
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Components.Init (initialState)
import Components.Typer (State, Query(..), Message(..))
import Components.View (render)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Maybe (Maybe(..))
import Halogen.VDom.Driver (runUI)

foreign import hot :: forall eff. Eff eff Unit


app :: Eff (HA.HalogenEffects ()) Unit
app = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI myButton unit body


main :: Unit
main = unsafePerformEff $ do
  app
  hot


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