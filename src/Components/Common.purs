module Components.Common where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Except (runExcept)
import Data.Either (Either(Left, Right))
import Data.Foreign (Foreign, readString)
import Data.Foreign.Index (readProp)
import Data.HTTP.Method (Method(GET))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.MediaType.Common (applicationJSON)
import Network.HTTP.Affjax (AJAX, AffjaxResponse, affjax, defaultRequest, post)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(ContentType, RequestHeader))
import Prelude (class Bind, ($), (<>))

type Ajax' e = (ajax :: AJAX | e)

postit :: ∀  p r e m. Bind m ⇒ MonadAff (Ajax' e) m ⇒ Requestable r ⇒ Respondable p ⇒ String → r → m (AffjaxResponse p)
postit url payload =
  liftAff $ post url payload

getit :: ∀  r e. Respondable r ⇒ Maybe String → String → Aff (Ajax' e) (AffjaxResponse r)
getit jwt url =
  affjax $
    defaultRequest
      { url     = url
      , method  = Left GET
      , headers =
        [ ContentType applicationJSON
        , RequestHeader "Authorization"
                      $ "Bearer " <> (fromMaybe "" jwt)
        ]
      }

parsetoken :: Foreign → String -> Maybe String
parsetoken resp prop =
  case runExcept $ readProp prop resp of
    Right fs ->
      case runExcept $ readString fs of
        Right s -> Just s
        _ -> Nothing
    _ -> Nothing
