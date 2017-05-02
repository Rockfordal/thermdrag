module Components.Container where

import Prelude (type (~>), Unit, ($), discard, pure, bind, unit)
import Components.Router as Router
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Var (($=))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen.Aff as HA
import Halogen (action, liftEff)
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import WebSocket as WS

wsUrl :: String
wsUrl = "ws://46.162.127.62:8081"

-- A producer coroutine that emits messages that arrive from the websocket.
wsProducer :: forall e . WS.Connection
                      -> CR.Producer String (Aff (avar :: AVAR, err :: EXCEPTION
                                                , ajax :: AJAX, ws  :: WS.WEBSOCKET | e)) Unit
wsProducer (WS.Connection socket) =
  CRA.produce \emit -> do
      socket.onmessage $= \event -> do
        emit $ Left $ WS.runMessage (WS.runMessageEvent event)


-- Consumer coroutine takes the `query` function from our component IO
-- record and sends `AddMessage` queries in when it receives inputs from producer
wsConsumer :: forall e. (Router.Input ~> Aff (HA.HalogenEffects e))
                  -> CR.Consumer String (Aff (HA.HalogenEffects e)) Unit
wsConsumer query =
  CR.consumer \msg -> do
    query $ action $ Router.IncomingMessage msg
    pure Nothing


-- Consumer coroutine takes output messages from our component IO, sends with websocket
wsSender :: forall e . WS.Connection -> CR.Consumer Router.Output
                      (Aff (HA.HalogenEffects (ws  :: WS.WEBSOCKET, ajax :: AJAX
                                             , err :: EXCEPTION | e))) Unit
wsSender (WS.Connection socket) = CR.consumer \msg -> do
  case msg of
    Router.OutputMessage msgContents -> do
        text <- pure $ msgContents
        liftEff $ socket.send (WS.Message text)
  pure Nothing


containerapp :: Eff (HA.HalogenEffects (ws  :: WS.WEBSOCKET, ajax :: AJAX
                                      , err :: EXCEPTION)) Unit
containerapp = do
  conn <- WS.newWebSocket (WS.URL wsUrl) []
  HA.runHalogenAff do
    body <- HA.awaitBody
    io   <- runUI Router.ui unit body
    _    <- forkAff $ Router.routeSignal io

    -- wsSender subscribes to our components output messages
    io.subscribe $ wsSender conn

    -- If we cant we can trigger an action
    -- io.query $ H.action $ B.Toggle

    -- We feed queries back to our component
    -- Connectiong consumer to producer
    -- Both will be initialized
    CR.runProcess (wsProducer conn CR.$$ wsConsumer io.query)