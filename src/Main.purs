module Main (main) where

import Prelude
import Components.Init (initialState)
import Components.Typer (Action, State)
import Components.Update (performAction)
import Components.View (render)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Thermite (Spec, defaultMain, simpleSpec)

foreign import hot :: forall eff. Eff eff Unit


main :: Unit
main = unsafePerformEff $ do
  defaultMain spec initialState unit
  hot


spec :: Spec _ State _ Action
spec = simpleSpec performAction render
