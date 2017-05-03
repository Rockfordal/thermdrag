module Components.Common where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (sessionStorage)
import DOM.WebStorage.Storage (Storage)
import Data.Either (Either(Left))
import Data.Foreign (Foreign, ForeignError, readString)
import Data.Foreign.Index (readProp)
import Data.HTTP.Method (Method(GET))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe, fromMaybe)
import Data.MediaType.Common (applicationJSON)
import Network.HTTP.Affjax (AJAX, AffjaxResponse, affjax, defaultRequest, post)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(ContentType, RequestHeader))
import Prelude (class Bind, bind, pure, ($), (<>), (=<<))

type Ajax' e = (ajax :: AJAX | e)


postit :: ∀  p r e m. Bind m ⇒ MonadAff (Ajax' e) m ⇒ Requestable r ⇒ Respondable p ⇒ String → r → m (AffjaxResponse p)
postit url payload =
  liftAff $ post url payload

getit :: ∀  r e. Respondable r ⇒ Maybe String → String → Aff (Ajax' e) (AffjaxResponse r)
getit jwt url =
  let
    tokenString = fromMaybe "" jwt
  in
    affjax $
      defaultRequest
        { url     = url
        , method  = Left GET
        , headers =
          [ ContentType applicationJSON
          , RequestHeader "Authorization"
                        $ "Bearer " <> tokenString
          ]
        }

parseResponse :: String -> Foreign -> Either (NonEmptyList ForeignError) String
parseResponse prop f =
  runExcept $ readString =<< readProp prop f

getSessionDb :: Eff (dom :: DOM) Storage
getSessionDb = do
  w <- window
  s <- sessionStorage w
  pure s
