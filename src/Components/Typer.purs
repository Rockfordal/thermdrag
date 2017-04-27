module Components.Typer where

-- import Data.Argonaut.Core (jsonEmptyObject)
-- import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
-- import Data.Monoid ((<>))

-- data Cred = Cred { username :: String, password :: String }

-- instance encodeJsonCred :: EncodeJson Cred where
--   encodeJson (Cred cred)
--     =  "username" := cred.username
--     ~> "password" := cred.password
--     ~> jsonEmptyObject

-- baseUrl :: String
-- baseUrl = "http://localhost:8083"

-- loginUrl :: String
-- loginUrl = baseUrl <> "/api/login"

wsUrl :: String
wsUrl = "ws://46.162.127.62:4000"