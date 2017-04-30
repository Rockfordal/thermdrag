module Main (main) where

import Prelude
import Components.Container (containerapp)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

foreign import hot :: forall eff. Eff eff Unit

main :: Unit
main = unsafePerformEff $ do
  containerapp
  hot