module Components.Helpers where

import Halogen.Themes.Bootstrap3 as B
import DOM.HTML.Indexed (HTMLdiv)
import Data.Array ((:))
import Data.String (toLower)
import Halogen.HTML (HTML, a, div, li_, text)
import Halogen.HTML.Elements (Node)
import Halogen.HTML.Properties (class_, href)
import Prelude ((<>))

container :: forall p i. Node HTMLdiv p i
container attrs =
  div (class_ B.container : attrs)

container_ :: forall p i. Array (HTML p i) -> HTML p i
container_ =
  container []

navlink :: forall p i. String -> HTML p i
navlink s =
  li_ [ a [ href ("#/" <> toLower s) ]
    [ text s ] ]