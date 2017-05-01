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

-- A producer coroutine that emits messages that arrive from the websocket.
wsProducer :: forall eff . WS.Connection
  -> CR.Producer String (Aff (avar :: AVAR, err :: EXCEPTION, ajax :: AJAX, ws :: WS.WEBSOCKET | eff)) Unit
wsProducer (WS.Connection socket) =
  CRA.produce \emit -> do
      socket.onmessage $= \event -> do
        emit $ Left $ WS.runMessage (WS.runMessageEvent event)


-- Consumer coroutine takes the `query` function from our component IO
-- record and sends `AddMessage` queries in when it receives inputs from producer
wsConsumer :: forall eff . (Router.Input ~> Aff (HA.HalogenEffects eff))
  -> CR.Consumer String (Aff (HA.HalogenEffects eff)) Unit
wsConsumer query =
  CR.consumer \msg -> do
    query $ action $ Router.IncomingMessage msg
    pure Nothing


-- Consumer coroutine takes output messages from our component IO, sends with websocket
wsSender :: forall eff . WS.Connection
  -> CR.Consumer Router.Message (Aff (HA.HalogenEffects (ws :: WS.WEBSOCKET, ajax :: AJAX, err :: EXCEPTION | eff))) Unit    
wsSender (WS.Connection socket) = CR.consumer \msg -> do
  case msg of
    Router.OutputMessage msgContents -> do
        text <- pure $ msgContents
        liftEff $ socket.send (WS.Message text)
  pure Nothing


containerapp :: Eff (HA.HalogenEffects (ws :: WS.WEBSOCKET, ajax :: AJAX, err :: EXCEPTION)) Unit
containerapp = do
  conn <- WS.newWebSocket (WS.URL wsUrl) []
  HA.runHalogenAff do
    body <- HA.awaitBody
    io   <- runUI Router.ui unit body
    _    <- forkAff $ Router.routeSignal io

    -- The wsSender consumer subscribes to all output messages from our component
    io.subscribe $ wsSender conn
    -- io.query $ H.action $ B.Toggle        

    -- Connecting the consumer to the producer initializes both,
    -- feeding queries back to our component as messages are received.
    CR.runProcess (wsProducer conn CR.$$ wsConsumer io.query)
    -- CR.runProcess (hashChangeProducer CR.$$ hashChangeConsumer io.query)


wsUrl :: String
wsUrl = "ws://46.162.127.62:8081"