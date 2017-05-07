module Main (main) where

import Components.Container (containerapp)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Prelude (Unit, ($), discard)

foreign import hot :: ∀ eff. Eff eff Unit

main :: Unit
main = unsafePerformEff $ do
  containerapp
  hot

